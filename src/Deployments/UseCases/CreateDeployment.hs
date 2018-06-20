module Deployments.UseCases.CreateDeployment
  ( Params(..)
  , Error(..)
  , call
  ) where

import RIO

import Deployments.Classes
import qualified Deployments.Domain.Build as Build
import qualified Deployments.Domain.Deployment as Deployment
import qualified Deployments.Domain.Environment as Environment

data Params = Params
  { build :: Build.Build
  , environment :: Environment.Environment
  }

data Error =
  ProjectsDontMatch

type UseCaseMonad m = (MonadIO m, DeploymentRepo m)

entity :: (MonadIO m) => Params -> m Deployment.QueuedDeployment
entity Params {..} = do
  let deploymentBuildId = Build.buildId build
  let deploymentEnvironmentId = Environment.environmentId environment
  deploymentId <- Deployment.genId
  return Deployment.QueuedDeployment {..}

create :: UseCaseMonad m => Params -> m Deployment.QueuedDeployment
create params = do
  deployment <- entity params
  createQueuedDeployment deployment
  return deployment

call :: UseCaseMonad m => Params -> m (Either Error Deployment.QueuedDeployment)
call params@(Params {..}) =
  let buildProjectId = Build.buildProjectId build
      environmentProjectId = Environment.environmentProjectId environment
   in case buildProjectId == environmentProjectId of
        False -> return $ Left ProjectsDontMatch
        True -> Right <$> create params
