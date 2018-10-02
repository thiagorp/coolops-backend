module Deployments.UseCases.CreateDeployment
  ( Params(..)
  , CallMonad
  , Error(..)
  , call
  ) where

import RIO

import Common.Database
import Deployments.Database.Deployment (createQueuedDeployment)
import qualified Deployments.Domain.Build as Build
import qualified Deployments.Domain.Deployment as Deployment
import qualified Deployments.Domain.Environment as Environment

data Params = Params
  { build :: !Build.Build
  , environment :: !Environment.Environment
  }

data Error =
  ProjectsDontMatch

type CallMonad m = (MonadIO m, HasPostgres m)

entity :: (MonadIO m) => Params -> m Deployment.QueuedDeployment
entity Params {..} = do
  let deploymentBuildId = Build.buildId build
  let deploymentEnvironmentId = Environment.environmentId environment
  deploymentId <- Deployment.genId
  return Deployment.QueuedDeployment {..}

create :: CallMonad m => Params -> m Deployment.QueuedDeployment
create params = do
  deployment <- entity params
  createQueuedDeployment deployment
  return deployment

call :: CallMonad m => Params -> m (Either Error Deployment.QueuedDeployment)
call params@Params {..} =
  let buildProjectId = Build.buildProjectId build
      environmentProjectId = Environment.environmentProjectId environment
   in if buildProjectId == environmentProjectId
        then Right <$> create params
        else return (Left ProjectsDontMatch)
