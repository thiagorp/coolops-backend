{-# LANGUAGE OverloadedStrings #-}

module Slack.UseCases.NotifyDeploymentQueuedByLock
  ( call
  ) where

import Import

import Database.Queries.DeploymentQueuedByLockMessageData (getMessageData)
import qualified Slack.Api.ChatMessages as Slack
import Slack.Api.Message


message ::
  Build
  -> Project
  -> Environment
  -> EnvironmentLock
  -> Message
message _ _ _ el = slackMessage { messageText = Just m }
  where
    m = "Your deployment is blocked by <@" <> lockCreatedBy <> ">'s lock"
    lockCreatedBy = environmentLockCreatedBy el


sendMessage ::
  Build
  -> Project
  -> Environment
  -> EnvironmentLock
  -> SlackAccessToken
  -> Text
  -> App ()
sendMessage b p e el sat tuId =
  void $ Slack.postMessage token tuId (message b p e el)
  where
    token = slackAccessTokenBotAccessToken sat


call :: DeploymentId -> EnvironmentLockId -> Text -> App ()
call dId elId tuId = do
  maybeData <- getMessageData dId elId
  case maybeData of
    Nothing ->
      return ()

    Just ( build, project, environment, environmentLock, slackAccessToken ) ->
      sendMessage
        (entityVal build)
        (entityVal project)
        (entityVal environment)
        (entityVal environmentLock)
        (entityVal slackAccessToken)
        tuId
