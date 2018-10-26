module Slack.Domain.AccessToken
  ( ID
  , AccessToken(..)
  , genId
  ) where

import RIO

import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow

import Auth.Domain (CompanyID)
import Util.Key

type ID = Key AccessToken

data AccessToken = AccessToken
  { tokenId :: !ID
  , tokenCompanyId :: !CompanyID
  , tokenTeamName :: !Text
  , tokenTeamId :: !Text
  , tokenScopes :: !Text
  , tokenUserAccessToken :: !Text
  , tokenBotAccessToken :: !Text
  , tokenBotUserId :: !Text
  } deriving (Generic)

instance FromRow AccessToken

instance ToRow AccessToken

genId :: MonadIO m => m ID
genId = genID
