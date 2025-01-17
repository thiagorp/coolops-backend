{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Slack.UseCases.NotifyNewEnvironmentLock
  ( Error(..)
  , call
  ) where

import Import

import Control.Monad.Except

import Database.Queries.EnvironmentLockMessageData
import qualified Slack.Api.ChatMessages as Slack
import Slack.Api.Message
import qualified Slack.MessageButtons as Button

data Error
  = MessageDataNotFound

saveSlackEnvironmentLockMessage :: EnvironmentLockId -> Text -> App ()
saveSlackEnvironmentLockMessage lockId slackMessageId = do
  now <- liftIO getCurrentTime
  void $
    insert $
      SlackEnvironmentLockMessage
        { slackEnvironmentLockMessageEnvironmentLockId = lockId
        , slackEnvironmentLockMessageSlackMessageId = slackMessageId
        , slackEnvironmentLockMessageCreatedAt = now
        , slackEnvironmentLockMessageUpdatedAt = now
        }


createMessage :: MessageData -> Message -> App ()
createMessage (Entity lockId _, _, _, _, Entity _ SlackProjectIntegration {..}, Entity _ SlackAccessToken {..}, _) m = do
  maybeResponseTs <- Slack.postMessage slackAccessTokenBotAccessToken slackProjectIntegrationChannelId m
  traverse_ (saveSlackEnvironmentLockMessage lockId) maybeResponseTs


updateMessage :: Text -> MessageData -> Message -> App ()
updateMessage slackMessageId (_, _, _, _, Entity _ SlackProjectIntegration {..}, Entity _ SlackAccessToken {..}, _) =
  Slack.updateMessage
    slackAccessTokenBotAccessToken
    slackProjectIntegrationChannelId
    slackMessageId


sendMessage :: MessageData -> Message -> App ()
sendMessage messageData@(_, _, _, _, _, _, maybeExsistingMessage) m =
  case maybeExsistingMessage of
    Nothing ->
      createMessage messageData m

    Just (Entity _ SlackEnvironmentLockMessage {..}) ->
      updateMessage slackEnvironmentLockMessageSlackMessageId messageData m


handleEntity :: (Monad m) => Error -> m (Maybe a) -> ExceptT Error m a
handleEntity e wrappedEntity = do
  entity <- lift wrappedEntity
  case entity of
    Nothing -> throwError e
    Just a -> return a

getMessageData_ :: CompanyId -> EnvironmentLockId -> ExceptT Error App MessageData
getMessageData_ cId lId = handleEntity MessageDataNotFound (getEnvironmentLockMessageData cId lId)

buildMessage :: MessageData -> Message
buildMessage (Entity lockId EnvironmentLock {..}, Entity _ Environment {..}, maybeBuild, Entity _ Project {..}, _, _, _) =
  slackMessage
    { messageText = Just messageText
    , messageAttachments = Just [ releaseLineAttachment ]
    }
  where
    messageText = "<@" <> environmentLockCreatedBy <> "> locked *" <> getValue projectName <> " - " <> getValue environmentName <> "*" <> buildInformation

    buildInformation =
      case maybeBuild of
        Nothing ->
          ""

        Just (Entity _ Build {..}) ->
          " for *" <> getValue buildName <> "*"

    releaseLineAttachment =
      case environmentLockReleasedBy of
        Nothing ->
          releaseButtonAttachment

        Just releasedBy ->
          releasedTextAttachment releasedBy

    releasedTextAttachment releasedBy =
      slackAttachment
        { attachmentText = Just ("<@" <> releasedBy <> "> unlocked it")
        }

    releaseButtonAttachment =
      slackAttachment
        { attachmentActions = Just [ releaseLockButton ]
        , attachmentCallbackId = Just (Button.actionToText (Button.ReleaseEnvironmentLock lockId))
        }

    releaseLockButton =
      slackAction
        { actionName = Button.actionToText (Button.ReleaseEnvironmentLock lockId)
        , actionText = "Unlock"
        , actionType = "button"
        }

call :: CompanyId -> EnvironmentLockId -> App (Either Error ())
call cId lId = runExceptT $ do
  messageData <- getMessageData_ cId lId
  lift $ sendMessage messageData (buildMessage messageData)
