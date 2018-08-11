module Slack.UseCases.NotifyNewBuild
  ( CallConstraint
  , Error(..)
  , call
  , message
  ) where

import RIO
import qualified RIO.Text as Text

import Control.Monad.Except
import Data.Time
import Data.UUID (toText)

import Common.Database (HasPostgres)
import Database.Queries.SlackBuildMessageData
import Deployments.Classes
import qualified Deployments.Domain.Build as B
import qualified Deployments.Domain.Project as P
import Slack.Api.ChatMessages
import Slack.Api.Message
import Slack.Classes
import Slack.Database.ProjectIntegration (getSlackIntegrationForProject)
import Slack.Domain.BuildMessage
import Slack.Domain.ProjectIntegration hiding (genId)

data Error
  = MessageDataNotFound
  | BuildNotFound
  | SlackConfigNotFound

saveSlackBuildMessage ::
     (SlackBuildMessageRepo m, MonadIO m) => B.ID -> Text -> m ()
saveSlackBuildMessage buildMessageBuildId buildMessageSlackMessageId = do
  buildMessageId <- genId
  createSlackBuildMessage BuildMessage {..}

createMessage ::
     (SlackClientMonad m, SlackBuildMessageRepo m)
  => ProjectIntegration
  -> B.Build
  -> Message
  -> m ()
createMessage ProjectIntegration {..} B.Build {..} m = do
  maybeResponseTs <- postMessage integrationAccessToken integrationChannelId m
  forM_ maybeResponseTs (saveSlackBuildMessage buildId)

sendMessage ::
     (SlackClientMonad m, SlackBuildMessageRepo m)
  => ProjectIntegration
  -> B.Build
  -> Message
  -> m ()
sendMessage config@ProjectIntegration {..} build@B.Build {..} m = do
  maybeExistingMesage <- getSlackBuildMessage buildId
  case maybeExistingMesage of
    Nothing -> createMessage config build m
    Just BuildMessage {..} ->
      updateMessage
        integrationAccessToken
        integrationChannelId
        buildMessageSlackMessageId
        m

handleEntity :: Monad m => Error -> m (Maybe a) -> ExceptT Error m a
handleEntity e wrappedEntity = do
  entity <- lift wrappedEntity
  case entity of
    Nothing -> throwError e
    Just a -> return a

getSlackConfig_ :: HasPostgres m => P.ID -> ExceptT Error m ProjectIntegration
getSlackConfig_ projectId =
  handleEntity SlackConfigNotFound (getSlackIntegrationForProject projectId)

getBuild_ :: BuildRepo m => P.CompanyID -> Text -> ExceptT Error m B.Build
getBuild_ cId bId = handleEntity BuildNotFound (getBuild cId bId)

type CallConstraint m
   = (HasPostgres m, BuildRepo m, SlackBuildMessageRepo m, SlackClientMonad m)

colorOf :: DbStatus -> Maybe Text
colorOf status =
  case status of
    DbQueued -> Nothing
    DbRunning -> Just "warning"
    DbSucceeded -> Just "good"
    DbFailed _ -> Just "danger"

buildMessage :: MessageData -> Message
buildMessage MessageData {..} =
  slackMessage
    {messageText = Just messageText, messageAttachments = Just attachments}
  where
    messageText =
      "*" <> dataProjectName <> "* has a new build: *" <> dataBuildName <> "*"
    attachments =
      [deploymentButtons] <> map buildDeploymentRow dataSlackDeployments
    deploymentButtons =
      slackAttachment
        { attachmentCallbackId = Just $ "deploy_build|" <> toText dataBuildId
        , attachmentType = Just "default"
        , attachmentActions = Just $ map buildAction dataEnvironments
        }
    buildDeploymentRow SlackDeployment {..} =
      slackAttachment
        { attachmentText =
            Just $
            "<@" <> deploymentUserId <> "> deployed to *" <>
            deploymentEnvironmentName <>
            "*"
        , attachmentFooter =
            Just $
            "<!date^" <>
            Text.pack (formatTime defaultTimeLocale "%s" deploymentTime) <>
            "^{date_pretty} - {time}|Deployment time conversion failed>"
        , attachmentColor = colorOf deploymentStatus
        }
    buildAction Environment {..} =
      slackAction
        { actionName = "environment"
        , actionText = "Deploy to " <> environmentName
        , actionType = "button"
        , actionValue = toText environmentId
        }

message_ :: HasPostgres m => P.CompanyID -> Text -> ExceptT Error m Message
message_ cId bId = do
  messageData <-
    handleEntity MessageDataNotFound (getSlackBuildMessageData cId bId)
  return $ buildMessage messageData

message :: HasPostgres m => P.CompanyID -> Text -> m (Either Error Message)
message cId bId = runExceptT $ message_ cId bId

call_ :: CallConstraint m => P.CompanyID -> Text -> ExceptT Error m ()
call_ cId bId = do
  build <- getBuild_ cId bId
  slackConfig <- getSlackConfig_ (B.buildProjectId build)
  m <- message_ cId bId
  lift $ sendMessage slackConfig build m

call :: CallConstraint m => P.CompanyID -> Text -> m (Either Error ())
call cId bId = runExceptT $ call_ cId bId
