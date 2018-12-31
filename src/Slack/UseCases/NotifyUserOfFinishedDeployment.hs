module Slack.UseCases.NotifyUserOfFinishedDeployment
  ( module Database.Queries.SlackDeploymentMessageData
  , Error(..)
  , call
  ) where

import Import

import Control.Monad.Except

import Common.Config (frontendBaseUrl)
import Database.Queries.SlackDeploymentMessageData
import Slack.Api.ChatMessages
import Slack.Api.Message
import Util.FrontendEndpoints (logsPage)

data Error =
  DataNotFound

handleEntity :: Error -> App (Maybe a) -> ExceptT Error App a
handleEntity e wrappedEntity = do
  entity <- lift wrappedEntity
  case entity of
    Nothing -> throwError e
    Just a -> return a

getMessageData_ :: CompanyId -> UUID -> ExceptT Error App MessageData
getMessageData_ companyId deploymentId =
  handleEntity DataNotFound (getSlackDeploymentMessageData companyId deploymentId)

sendMessage :: (Text, Text) -> Message -> ExceptT Error App ()
sendMessage (userId, accessToken) message = void $ lift $ postMessage accessToken userId message

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

call_ :: CompanyId -> UUID -> ExceptT Error App ()
call_ companyId deploymentId = do
  appBaseUrl <- frontendBaseUrl
  messageData <- getMessageData_ companyId deploymentId
  let message = buildMessage appBaseUrl messageData
      Entity _ SlackDeployment {..} = dataSlackDeployment messageData
      Entity _ SlackAccessToken {..} = dataSlackAccessToken messageData
  sendMessage (slackDeploymentSlackUserId, slackAccessTokenBotAccessToken) message

call :: CompanyId -> UUID -> App (Either Error ())
call companyId deploymentId = runExceptT $ call_ companyId deploymentId
