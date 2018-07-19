module Slack.Api.ChatMessages
  ( SlackClientMonad
  , postMessage
  ) where

import RIO

import Slack.Api.ClientBase
import Slack.Api.Message

postMessage :: SlackClientMonad m => Text -> Text -> Message -> m ()
postMessage token channel message =
  void $ slackRequest (PostMessageAsBot params)
  where
    params = ChatPostMessageAsBot token channel message
