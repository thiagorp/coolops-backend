module Slack.Api.IncomingWebhooks
  ( SlackClientMonad
  , sendIncomingWebhook
  ) where

import RIO

import Slack.Api.ClientBase
import Slack.Api.Message

sendIncomingWebhook :: (SlackClientMonad m) => Text -> Message -> m ()
sendIncomingWebhook url message =
  slackRequest (IncomingWebhook url message) >> return ()