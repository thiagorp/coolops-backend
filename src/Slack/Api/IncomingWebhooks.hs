module Slack.Api.IncomingWebhooks
  ( SlackClientMonad
  , sendIncomingWebhook
  ) where

import Import

import Slack.Api.ClientBase
import Slack.Api.Message

sendIncomingWebhook :: Text -> Message -> App ()
sendIncomingWebhook url message =
  void $ slackRequest (IncomingWebhook url message)
