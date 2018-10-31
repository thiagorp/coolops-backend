module Deployments.UseCases.RunNextDeployment
  ( Error(..)
  , CallMonad
  , call
  ) where

import RIO

import Control.Monad.Except

import qualified BackgroundJobs.AppJobs as Background
import Common.PersistDatabase
import Deployments.Database.Deployment
import Deployments.Domain.Deployment
import Deployments.Gateway.Kubernetes

data Error
  = NoDeploymentToRun
  | MissingEntities
  | FailedToRunJob
  deriving (Show)

type CallMonad m = (MonadIO m, RunDeploymentMonad m, Background.NotifyBuildConstraint m)

type RunMonad m a = ExceptT Error (Db m) a

runNext :: (MonadIO m, RunDeploymentMonad m) => Entity Deployment -> DeploymentResources -> RunMonad m ()
runNext deployment resources = do
  result <- lift $ lift $ runDeployment deployment resources
  now <- liftIO getCurrentTime
  lift $
    update
      (entityKey deployment)
      [DeploymentStartedAt =. Just now, DeploymentStatus =. Running, DeploymentUpdatedAt =. now]
  if result
    then return ()
    else lift transactionUndo >> throwError FailedToRunJob

getDeploymentResources_ :: (MonadIO m) => Entity Deployment -> RunMonad m DeploymentResources
getDeploymentResources_ deployment = handleEntity MissingEntities $ getDeploymentsResources deployment

getNextQueuedDeployment_ :: (MonadIO m) => CompanyId -> RunMonad m (Entity Deployment)
getNextQueuedDeployment_ cId = handleEntity NoDeploymentToRun $ getNextQueuedDeployment cId

notify :: (CallMonad m) => DeploymentResources -> RunMonad m ()
notify DeploymentResources {..} = void $ lift $ Background.notifyBuild projectCompanyId buildKey
  where
    Project {..} = entityVal deploymentProject
    BuildKey buildKey = entityKey deploymentBuild

call_ :: CallMonad m => CompanyId -> RunMonad m ()
call_ companyId = do
  deployment <- getNextQueuedDeployment_ companyId
  resources <- getDeploymentResources_ deployment
  running <- runNext deployment resources
  notify resources
  return running

call :: CallMonad m => CompanyId -> m (Either Error ())
call = runDb . runExceptT . call_

handleEntity :: (Monad m) => Error -> Db m (Maybe a) -> RunMonad m a
handleEntity e wrappedEntity = do
  entity <- lift wrappedEntity
  case entity of
    Nothing -> throwError e
    Just a -> return a
