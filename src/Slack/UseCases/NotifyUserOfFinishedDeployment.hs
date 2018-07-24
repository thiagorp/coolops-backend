module Slack.UseCases.NotifyUserOfFinishedDeployment
  ( CallConstraint
  , Error(..)
  , call
  ) where

import RIO

import Control.Monad.Except

import Common.Database (HasPostgres)
import Database.Queries.SlackDeploymentMessageData
import Deployments.Domain.Project (CompanyID)
import Slack.Api.ChatMessages
import Slack.Api.Message

data Error =
  DataNotFound

type CallConstraint m = (HasPostgres m, SlackClientMonad m)

handleEntity :: Monad m => Error -> m (Maybe a) -> ExceptT Error m a
handleEntity e wrappedEntity = do
  entity <- lift wrappedEntity
  case entity of
    Nothing -> throwError e
    Just a -> return a

getMessageData_ ::
     HasPostgres m => CompanyID -> Text -> ExceptT Error m MessageData
getMessageData_ companyId deploymentId =
  handleEntity
    DataNotFound
    (getSlackDeploymentMessageData companyId deploymentId)

sendMessage ::
     SlackClientMonad m => (Text, Text) -> Message -> ExceptT Error m ()
sendMessage (userId, accessToken) message =
  void . lift $ postMessage accessToken userId message

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
    messageText = "_Deployment status update_"
    attachments = [notificationMessage]
    notificationMessage =
      slackAttachment
        { attachmentText = Just attachmentText
        , attachmentMarkdown = Just ["text"]
        , attachmentColor = colorOf dataDbStatus
        }
    attachmentText =
      "Your *" <> dataBuildName <> "* deployment to *" <> dataProjectName <>
      "* - *" <>
      dataEnvironmentName <>
      "* has finished"

call_ :: CallConstraint m => CompanyID -> Text -> ExceptT Error m ()
call_ companyId deploymentId = do
  messageData <- getMessageData_ companyId deploymentId
  let message = buildMessage messageData
  sendMessage (dataSendingParams messageData) message

call :: CallConstraint m => CompanyID -> Text -> m (Either Error ())
call companyId deploymentId = runExceptT $ call_ companyId deploymentId
