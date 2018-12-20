module Slack.UseCases.NotifyUserOfFinishedDeployment
  ( module Database.Queries.SlackDeploymentMessageData
  , CallConstraint
  , Error(..)
  , call
  ) where

import RIO

import Control.Monad.Except
import Data.Time

import Common.Config (frontendBaseUrl)
import Database.Queries.SlackDeploymentMessageData
import Slack.Api.ChatMessages
import Slack.Api.Message
import Util.FrontendEndpoints (logsPage)

data Error =
  DataNotFound

type CallConstraint m = (HasDb m, SlackClientMonad m, HasEnv m)

handleEntity :: (MonadIO m) => Error -> Db m (Maybe a) -> ExceptT Error (Db m) a
handleEntity e wrappedEntity = do
  entity <- lift wrappedEntity
  case entity of
    Nothing -> throwError e
    Just a -> return a

getMessageData_ :: HasDb m => CompanyId -> UUID -> ExceptT Error (Db m) MessageData
getMessageData_ companyId deploymentId =
  handleEntity DataNotFound (getSlackDeploymentMessageData companyId deploymentId)

sendMessage :: (SlackClientMonad m) => (Text, Text) -> Message -> ExceptT Error (Db m) ()
sendMessage (userId, accessToken) message = void $ lift $ lift $ postMessage accessToken userId message

colorOf :: DeploymentStatus -> Maybe Text
colorOf status =
  case status of
    Queued -> Nothing
    Running -> Just "warning"
    Succeeded -> Just "good"
    Failed _ -> Just "danger"

buildMessage :: Text -> MessageData -> Message
buildMessage appBaseUrl MessageData {..} =
  slackMessage
    { messageText = Just messageText
    , messageAttachments = Just attachments
    }
  where
    Entity _ Build {..} = dataBuild
    Entity (DeploymentKey deploymentId) Deployment {..} = dataDeployment
    Entity _ Environment {..} = dataEnvironment
    Entity _ Project {..} = dataProject
    messageText = "_Deployment status update_"
    attachments = [notificationMessage]
    notificationMessage =
      slackAttachment
        { attachmentText = Just attachmentText
        , attachmentMarkdown = Just ["text"]
        , attachmentColor = colorOf deploymentStatus
        , attachmentFooter =
            Just $ "<" <> appBaseUrl <> logsPage (uuidToText deploymentId) <> "|See logs>"
        }
    attachmentText =
      "Your *" <> getValue buildName <> "* deployment to *" <> getValue projectName <> "* - *" <>
      getValue environmentName <>
      "* has finished"

call_ :: (CallConstraint m) => CompanyId -> UUID -> ExceptT Error (Db m) ()
call_ companyId deploymentId = do
  appBaseUrl <- frontendBaseUrl
  messageData <- getMessageData_ companyId deploymentId
  let message = buildMessage appBaseUrl messageData
      Entity _ SlackDeployment {..} = dataSlackDeployment messageData
      Entity _ SlackAccessToken {..} = dataSlackAccessToken messageData
  sendMessage (slackDeploymentSlackUserId, slackAccessTokenBotAccessToken) message

call :: (CallConstraint m) => CompanyId -> UUID -> m (Either Error ())
call companyId deploymentId = runDb $ runExceptT $ call_ companyId deploymentId
