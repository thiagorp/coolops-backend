module Deployments.UseCases.CreateDeployment
  ( module Deployments.Database.Deployment
  , Params(..)
  , Error(..)
  , call
  ) where

import Import

import Deployments.Database.Deployment
import qualified Deployments.UseCases.LockEnvironment as LockEnvironment

data Params = Params
  { build :: !(Entity Build)
  , environment :: !(Entity Environment)
  , userId :: !Text
  }

data Error =
  ProjectsDontMatch

entity :: Params -> App Deployment
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


lockEnvironment :: Params -> App ()
lockEnvironment Params {..} =
  let (Entity environmentId Environment {..}) = environment
   in when environmentLockOnDeployment $
        void (LockEnvironment.call environmentId userId)


create :: Params -> App (Entity Deployment)
create params = do
  deployment <- entity params
  deploymentId <- runDb $ insert deployment
  lockEnvironment params
  return (Entity deploymentId deployment)


call :: Params -> App (Either Error (Entity Deployment))
call params@Params {..} =
  let (Entity _ Build {..}) = build
      (Entity _ Environment {..}) = environment
   in if buildProjectId == environmentProjectId
        then Right <$> create params
        else return (Left ProjectsDontMatch)
