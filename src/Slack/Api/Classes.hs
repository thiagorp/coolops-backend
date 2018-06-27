module Slack.Api.Classes where

import RIO

class HasSlackSettings m where
  slackClientId :: m Text
  slackClientSecret :: m ByteString
  slackVerificationToken :: m Text
