module Deployments.Domain.Deployment
  ( QueuedDeployment(..)
  , RunningDeployment(..)
  , FinishedDeployment(..)
  , ID
  , BuildID
  , EnvironmentID
  , Status(..)
  , UnknownReason(..)
  , FailedReason(..)
  , genId
  , run
  ) where

import RIO

import Data.Time (UTCTime, getCurrentTime)

import qualified Deployments.Domain.Build as Build
import qualified Deployments.Domain.Environment as Environment
import Util.Key

type ID = Key QueuedDeployment

type BuildID = Build.ID

type EnvironmentID = Environment.ID

data UnknownReason =
  JobNotFound

data FailedReason
  = InvalidDockerImage
  | JobFailed

data Status
  = Succeeded
  | Failed FailedReason
  | Unknown UnknownReason

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

run :: (MonadIO m) => QueuedDeployment -> m RunningDeployment
run QueuedDeployment {..} = do
  currentTime <- liftIO getCurrentTime
  return
    RunningDeployment
      {runningDeploymentId = deploymentId, deploymentStartedAt = currentTime}

genId :: MonadIO m => m ID
genId = genID
