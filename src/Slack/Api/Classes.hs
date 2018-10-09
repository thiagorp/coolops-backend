module Slack.Api.Classes where

import RIO

import qualified Common.Config as Config
import Env

class HasSlackSettings m where
  slackClientId :: m Text
  slackClientSecret :: m ByteString
  slackVerificationToken :: m Text
  slackSigningSecret :: m ByteString

instance HasSlackSettings (RIO Env) where
  slackClientId = Config.slackClientId <$> asks slackSettings
  slackClientSecret = Config.slackClientSecret <$> asks slackSettings
  slackVerificationToken = Config.slackVerificationToken <$> asks slackSettings
  slackSigningSecret = Config.slackSigningSecret <$> asks slackSettings
