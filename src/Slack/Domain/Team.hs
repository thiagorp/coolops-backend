module Slack.Domain.Team
  ( ID
  , IncomingWebhookConfig(..)
  , BotUser(..)
  , Team(..)
  , genId
  ) where

import RIO

import Auth.Domain (CompanyID)
import Util.Key

type ID = Key Team

data IncomingWebhookConfig = IncomingWebhookConfig
  { webhookUrl :: !Text
  , webhookChannel :: !Text
  , webhookConfigurationUrl :: !Text
  }

data BotUser = BotUser
  { botUserId :: !Text
  , botUserAccessToken :: !Text
  }

data Team = Team
  { teamId :: !ID
  , teamCompanyId :: !CompanyID
  , teamName :: !Text
  , teamSlackId :: !Text
  , teamAccessToken :: !Text
  , teamIncomingWebhook :: !IncomingWebhookConfig
  , teamBotUser :: !BotUser
  }

genId :: MonadIO m => m ID
genId = genID
