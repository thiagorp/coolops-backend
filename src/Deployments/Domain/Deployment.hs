module Deployments.Domain.Deployment
  ( module Model
  , DeploymentResources(..)
  , finish
  ) where

import RIO

import Data.Time (getCurrentTime)

import Model

data DeploymentResources = DeploymentResources
  { deploymentProject :: !(Entity Project)
  , deploymentEnvironment :: !(Entity Environment)
  , deploymentBuild :: !(Entity Build)
  , deploymentUserId :: !Text
  }

finish :: (MonadIO m) => DeploymentStatus -> Deployment -> m Deployment
finish status deployment = do
  currentTime <- liftIO getCurrentTime
  return deployment {deploymentFinishedAt = Just currentTime, deploymentStatus = status}
