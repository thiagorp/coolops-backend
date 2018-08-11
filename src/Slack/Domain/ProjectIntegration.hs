module Slack.Domain.ProjectIntegration
  ( ID
  , ProjectIntegration(..)
  , ProjectID
  , Scopes
  , genId
  ) where

import RIO

import Data.Aeson

import qualified Deployments.Domain.Project as Project
import Util.Key

type ID = Key ProjectIntegration

type ProjectID = Project.ID

type Scopes = Value

data ProjectIntegration = ProjectIntegration
  { integrationId :: !ID
  , integrationProjectId :: !ProjectID
  , integrationAccessToken :: !Text
  , integrationWorkspaceName :: !Text
  , integrationAppId :: !Text
  , integrationAppUserId :: !Text
  , integrationInstallerUserId :: !Text
  , integrationAuthorizingUserId :: !Text
  , integrationTeamId :: !Text
  , integrationChannelId :: !Text
  , integrationScopes :: !Scopes
  }

genId :: MonadIO m => m ID
genId = genID
