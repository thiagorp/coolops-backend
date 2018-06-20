module Deployments.Domain.Deployment
  ( QueuedDeployment(..)
  , RunningDeployment(..)
  , FinishedDeployment(..)
  , ID
  , BuildID
  , EnvironmentID
  , Status(..)
  , genId
  ) where

import RIO

import Data.Time (UTCTime)

import qualified Deployments.Domain.Build as Build
import qualified Deployments.Domain.Environment as Environment
import Util.Key

type ID = Key QueuedDeployment

type BuildID = Build.ID

type EnvironmentID = Environment.ID

data Status
  = Succeeded
  | Failed

data QueuedDeployment = QueuedDeployment
  { deploymentId :: !ID
  , deploymentBuildId :: !BuildID
  , deploymentEnvironmentId :: !EnvironmentID
  }

data RunningDeployment = RunningDeployment
  { runningDeploymentId :: !ID
  , deploymentStartedAt :: !UTCTime
  }

data FinishedDeployment = FinishedDeployment
  { finishedDeploymentId :: !ID
  , deploymentFinishedAt :: !UTCTime
  , deploymentStatus :: !Status
  }

genId :: MonadIO m => m ID
genId = genID
