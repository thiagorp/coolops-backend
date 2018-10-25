module Slack.Domain.ProjectIntegration
  ( ID
  , ProjectIntegration(..)
  , ProjectID
  , genId
  ) where

import RIO

import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow

import qualified Deployments.Domain.Project as Project
import Util.Key

type ID = Key ProjectIntegration

type ProjectID = Project.ID

data ProjectIntegration = ProjectIntegration
  { integrationId :: !ID
  , integrationProjectId :: !ProjectID
  , integrationChannelId :: !Text
  , integrationChannelName :: !Text
  } deriving (Generic)

instance FromRow ProjectIntegration

instance ToRow ProjectIntegration

genId :: MonadIO m => m ID
genId = genID
