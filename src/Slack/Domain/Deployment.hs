module Slack.Domain.Deployment
  ( ID
  , BuildMessageID
  , DeploymentID
  , Deployment(..)
  , genId
  ) where

import RIO

import Data.Time

import qualified Deployments.Domain.Deployment as Deployment
import qualified Slack.Domain.BuildMessage as BuildMessage
import Util.Key

type ID = Key Deployment

type BuildMessageID = BuildMessage.ID

type DeploymentID = Deployment.ID

data Deployment = Deployment
  { deploymentId :: !ID
  , deploymentBuildMessageId :: !BuildMessageID
  , deploymentDeploymentId :: !DeploymentID
  , deploymentSlackUserName :: !Text
  , deploymentSlackUserId :: !Text
  , deploymentDeployedAt :: !UTCTime
  }

genId :: MonadIO m => m ID
genId = genID
