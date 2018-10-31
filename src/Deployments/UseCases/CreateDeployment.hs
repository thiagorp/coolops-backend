module Deployments.UseCases.CreateDeployment
  ( module Deployments.Database.Deployment
  , Params(..)
  , CallMonad
  , Error(..)
  , call
  ) where

import RIO

import Deployments.Database.Deployment

data Params = Params
  { build :: !(Entity Build)
  , environment :: !(Entity Environment)
  }

data Error =
  ProjectsDontMatch

type CallMonad m = (MonadIO m, HasDb m)

entity :: (MonadIO m) => Params -> m Deployment
entity Params {..} = do
  now <- liftIO getCurrentTime
  let (Entity buildId _) = build
  let (Entity environmentId _) = environment
  let deploymentBuildId = buildId
  let deploymentEnvironmentId = environmentId
  let deploymentStatus = Queued
  let deploymentStartedAt = Nothing
  let deploymentFinishedAt = Nothing
  let deploymentCreatedAt = now
  let deploymentUpdatedAt = now
  return Deployment {..}

create :: (CallMonad m) => Params -> m (Entity Deployment)
create params = do
  deployment <- entity params
  deploymentId <- runDb $ insert deployment
  return (Entity deploymentId deployment)

call :: (CallMonad m) => Params -> m (Either Error (Entity Deployment))
call params@Params {..} =
  let (Entity _ Build {..}) = build
      (Entity _ Environment {..}) = environment
   in if buildProjectId == environmentProjectId
        then Right <$> create params
        else return (Left ProjectsDontMatch)
