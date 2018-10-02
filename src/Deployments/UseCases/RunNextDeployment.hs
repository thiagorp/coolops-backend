module Deployments.UseCases.RunNextDeployment
  ( Error(..)
  , CallMonad
  , call
  ) where

import RIO

import Control.Monad.Except

import qualified BackgroundJobs.AppJobs as Background
import Common.Database
import Deployments.Database.Deployment (getDeploymentResources, getNextQueuedDeployment, saveRunningDeployment)
import Deployments.Domain.Deployment
import Deployments.Domain.Project (CompanyID)
import Deployments.Gateway.Kubernetes
import Util.Key

data Error
  = NoDeploymentToRun
  | MissingEntities
  | FailedToRunJob

type CallMonad m = (HasDBTransaction m, HasPostgres m, RunDeploymentMonad m, Background.NotifyBuildConstraint m)

type RunMonad m a = ExceptT Error m a

runNext ::
     (HasPostgres m, RunDeploymentMonad m) => QueuedDeployment -> DeploymentResources -> RunMonad m RunningDeployment
runNext queued resources = do
  running <- run queued
  result <- lift $ runDeployment queued resources
  lift $ saveRunningDeployment running
  if result
    then return running
    else throwError FailedToRunJob

getDeploymentResources_ :: HasPostgres m => CompanyID -> QueuedDeployment -> RunMonad m DeploymentResources
getDeploymentResources_ cId QueuedDeployment {..} =
  handleEntity MissingEntities $
  getDeploymentResources cId (keyText deploymentEnvironmentId) (keyText deploymentBuildId)

getNextQueuedDeployment_ :: HasPostgres m => CompanyID -> RunMonad m QueuedDeployment
getNextQueuedDeployment_ cId = handleEntity NoDeploymentToRun $ getNextQueuedDeployment cId

call_ :: CallMonad m => CompanyID -> RunMonad m RunningDeployment
call_ companyId = do
  deployment <- getNextQueuedDeployment_ companyId
  resources <- getDeploymentResources_ companyId deployment
  running <- runNext deployment resources
  lift $ Background.notifyBuild companyId (keyText $ deploymentBuildId deployment)
  return running

call :: CallMonad m => CompanyID -> m (Either Error RunningDeployment)
call = runTransaction . runExceptT . call_

handleEntity :: (Monad m) => Error -> m (Maybe a) -> RunMonad m a
handleEntity e wrappedEntity = do
  entity <- lift wrappedEntity
  case entity of
    Nothing -> throwError e
    Just a -> return a
