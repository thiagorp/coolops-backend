module Deployments.Domain.Deployment
  ( QueuedDeployment(..)
  , RunningDeployment(..)
  , FinishedDeployment(..)
  , DeploymentResources(..)
  , ID
  , BuildID
  , EnvironmentID
  , Status(..)
  , FailedReason(..)
  , genId
  , run
  , finish
  ) where

import RIO

import Data.Time (UTCTime, getCurrentTime)

import qualified Deployments.Domain.Build as Build
import qualified Deployments.Domain.Environment as Environment
import qualified Deployments.Domain.Project as Project
import Util.Key

type ID = Key QueuedDeployment

type BuildID = Build.ID

type EnvironmentID = Environment.ID

data FailedReason
  = InvalidDockerImage
  | JobFailed
  | JobNotFound

data Status
  = Succeeded
  | Failed FailedReason

data DeploymentResources = DeploymentResources
  { deploymentProject :: !Project.Project
  , deploymentEnvironment :: !Environment.Environment
  , deploymentBuild :: !Build.Build
  }

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

finish :: (MonadIO m) => Status -> RunningDeployment -> m FinishedDeployment
finish status RunningDeployment {..} = do
  currentTime <- liftIO getCurrentTime
  return
    FinishedDeployment
      { finishedDeploymentId = runningDeploymentId
      , deploymentFinishedAt = currentTime
      , deploymentStatus = status
      }

genId :: MonadIO m => m ID
genId = genID
