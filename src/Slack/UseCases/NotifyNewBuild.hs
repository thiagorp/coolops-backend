module Slack.UseCases.NotifyNewBuild
  ( module Database.Queries.SlackBuildMessageData
  , CallConstraint
  , Error(..)
  , call
  , message
  ) where

import RIO
import qualified RIO.HashMap as HashMap
import qualified RIO.Text as Text

import Control.Monad.Except
import Data.Time
import Data.UUID (toText)

import Common.Config (frontendBaseUrl)
import Database.Queries.SlackBuildMessageData
import Deployments.Database.Build (getBuild)
import Slack.Api.ChatMessages
import Slack.Api.Message
import qualified Slack.Database.AccessToken as AT
import Slack.Database.BuildMessage (getSlackBuildMessage)
import qualified Slack.Database.ProjectIntegration as PI
import Util.FrontendEndpoints (logsPage)

data Error
  = MessageDataNotFound
  | BuildNotFound
  | SlackConfigNotFound
  | SlackAccessTokenNotFound

saveSlackBuildMessage :: (MonadIO m) => BuildId -> Text -> Db m ()
saveSlackBuildMessage slackBuildMessageBuildId slackBuildMessageSlackMessageId = do
  now <- liftIO getCurrentTime
  let slackBuildMessageCreatedAt = now
  let slackBuildMessageUpdatedAt = now
  void $ insert SlackBuildMessage {..}

createMessage ::
     (SlackClientMonad m)
  => Entity SlackProjectIntegration
  -> Entity SlackAccessToken
  -> Entity Build
  -> Message
  -> Db m ()
createMessage (Entity _ SlackProjectIntegration {..}) (Entity _ SlackAccessToken {..}) (Entity buildId _) m = do
  maybeResponseTs <- lift $ postMessage slackAccessTokenBotAccessToken slackProjectIntegrationChannelId m
  forM_ maybeResponseTs (saveSlackBuildMessage buildId)

sendMessage ::
     (SlackClientMonad m)
  => Entity SlackProjectIntegration
  -> Entity SlackAccessToken
  -> Entity Build
  -> Message
  -> Db m ()
sendMessage config@(Entity _ SlackProjectIntegration {..}) at@(Entity _ SlackAccessToken {..}) build@(Entity buildId Build {..}) m = do
  maybeExistingMesage <- getSlackBuildMessage buildId
  case maybeExistingMesage of
    Nothing -> createMessage config at build m
    Just (Entity _ SlackBuildMessage {..}) ->
      lift $
      updateMessage slackAccessTokenBotAccessToken slackProjectIntegrationChannelId slackBuildMessageSlackMessageId m

handleEntity :: (Monad m) => Error -> m (Maybe a) -> ExceptT Error m a
handleEntity e wrappedEntity = do
  entity <- lift wrappedEntity
  case entity of
    Nothing -> throwError e
    Just a -> return a

getSlackConfig_ :: (MonadIO m) => ProjectId -> ExceptT Error (Db m) (Entity SlackProjectIntegration)
getSlackConfig_ projectId = handleEntity SlackConfigNotFound (PI.findByProjectId projectId)

getSlackAccessToken_ :: (MonadIO m) => ProjectId -> ExceptT Error (Db m) (Entity SlackAccessToken)
getSlackAccessToken_ projectId = handleEntity SlackConfigNotFound (AT.findByProjectId projectId)

getBuild_ :: (MonadIO m) => CompanyId -> UUID -> ExceptT Error (Db m) (Entity Build)
getBuild_ cId bId = handleEntity BuildNotFound (getBuild cId bId)

type CallConstraint m = (MonadUnliftIO m, SlackClientMonad m, HasDb m)

colorOf :: DeploymentStatus -> Maybe Text
colorOf status =
  case status of
    Queued -> Nothing
    Running -> Just "warning"
    Succeeded -> Just "good"
    Failed _ -> Just "danger"

buildMessage :: Text -> MessageData -> Message
buildMessage appBaseUrl MessageData {..} =
  slackMessage {messageText = Just messageText, messageAttachments = Just attachments}
  where
    (Entity (BuildKey buildId) Build {..}, Entity _ Project {..}, _) = dataBuildInfo
    messageText = "*" <> getValue projectName <> "* has a new build: *" <> getValue buildName <> "*"
    attachments =
      [deploymentButtons] <> [buildMetadataRow (HashMap.toList buildMetadata)] <> map buildDeploymentRow dataDeployments
    deploymentButtons =
      slackAttachment
        { attachmentCallbackId = Just $ "deploy_build|" <> toText buildId
        , attachmentType = Just "default"
        , attachmentActions = Just $ map buildAction dataEnvironments
        }
    buildMetadataRow fields =
      slackAttachment
        { attachmentMarkdown = Just ["fields"]
        , attachmentFields = Just $ map (\(t, v) -> slackField {fieldValue = v, fieldTitle = t}) fields
        }
    buildDeploymentRow (Entity _ SlackDeployment {..}, Entity _ Environment {..}, Entity (DeploymentKey deploymentId) Deployment {..}) =
      slackAttachment
        { attachmentText =
            Just $
              case deploymentStatus of
                Succeeded -> "<@" <> slackDeploymentSlackUserId <> "> deployed to *" <> getValue environmentName <> "*"
                Failed _ -> "<@" <> slackDeploymentSlackUserId <> "> deployed to *" <> getValue environmentName <> "*"
                Queued -> "<@" <> slackDeploymentSlackUserId <> "> has a queued deployment to *" <> getValue environmentName <> "*"
                Running -> "<@" <> slackDeploymentSlackUserId <> "> has a running deployment to *" <> getValue environmentName <> "*"
        , attachmentFooter =
            Just $
            "<!date^" <> Text.pack (formatTime defaultTimeLocale "%s" slackDeploymentDeployedAt) <>
            "^{date_pretty} - {time}|Deployment time conversion failed> | " <>
            "<" <>
            appBaseUrl <>
            logsPage (uuidToText deploymentId) <>
            "|See logs>"
        , attachmentColor = colorOf deploymentStatus
        }
    buildAction (Entity (EnvironmentKey environmentId) Environment {..}) =
      slackAction
        { actionName = "environment"
        , actionText = "Deploy to " <> getValue environmentName
        , actionType = "button"
        , actionValue = toText environmentId
        }

message_ :: (MonadIO m) => CompanyId -> UUID -> ExceptT Error (Db m) Message
message_ cId bId = do
  messageData <- handleEntity MessageDataNotFound (getSlackBuildMessageData cId bId)
  appBaseUrl <- frontendBaseUrl
  return $ buildMessage appBaseUrl messageData

message :: (CallConstraint m) => CompanyId -> UUID -> m (Either Error Message)
message cId bId = runDb $ runExceptT $ message_ cId bId

call_ :: (CallConstraint m) => CompanyId -> UUID -> ExceptT Error (Db m) ()
call_ cId bId = do
  build@(Entity _ Build {..}) <- getBuild_ cId bId
  slackConfig <- getSlackConfig_ buildProjectId
  slackAccessToken <- getSlackAccessToken_ buildProjectId
  m <- message_ cId bId
  lift $ sendMessage slackConfig slackAccessToken build m

call :: (CallConstraint m) => CompanyId -> UUID -> m (Either Error ())
call cId bId = runDb $ runExceptT $ call_ cId bId
