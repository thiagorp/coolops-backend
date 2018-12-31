module Deployments.UseCases.RunNextDeployment
  ( Error(..)
  , call
  ) where

import Import

import Control.Monad.Except

import qualified BackgroundJobs.AppJobs as Background
import Deployments.Database.Deployment
import Deployments.Domain.Deployment
import Deployments.Gateway.Kubernetes
import qualified Deployments.UseCases.LockEnvironment as LockEnvironment

data Error
  = NoDeploymentToRun
  | MissingEntities
  | FailedToRunJob
  deriving (Show)

type RunMonad = ExceptT Error App


lockEnvironment :: DeploymentResources -> App ()
lockEnvironment DeploymentResources {..} =
  let (Entity environmentId Environment {..}) = deploymentEnvironment
      userId = deploymentUserId
   in when environmentLockOnDeployment $
        void (LockEnvironment.call environmentId userId)


runNext :: Entity Deployment -> DeploymentResources -> RunMonad ()
runNext deployment resources = do
  result <- lift $ runDeployment deployment resources
  now <- liftIO getCurrentTime
  lift $ lockEnvironment resources
  lift $
    update
      (entityKey deployment)
      [DeploymentStartedAt =. Just now, DeploymentStatus =. Running, DeploymentUpdatedAt =. now]
  if result
    then return ()
    else lift transactionUndo >> throwError FailedToRunJob


getDeploymentResources_ :: Entity Deployment -> RunMonad DeploymentResources
getDeploymentResources_ deployment = handleEntity MissingEntities $ getDeploymentsResources deployment


getNextQueuedDeployment_ :: CompanyId -> RunMonad (Entity Deployment)
getNextQueuedDeployment_ cId = handleEntity NoDeploymentToRun $ getNextQueuedDeployment cId


notify :: DeploymentResources -> RunMonad ()
notify DeploymentResources {..} = void $ lift $ Background.notifyBuild projectCompanyId buildKey
  where
    Project {..} = entityVal deploymentProject
    BuildKey buildKey = entityKey deploymentBuild


call :: CompanyId -> App (Either Error ())
call companyId = runExceptT $ do
  deployment <- getNextQueuedDeployment_ companyId
  resources <- getDeploymentResources_ deployment
  running <- runNext deployment resources
  notify resources
  return running


handleEntity :: Error -> App (Maybe a) -> RunMonad a
handleEntity e wrappedEntity = do
  entity <- lift wrappedEntity
  case entity of
    Nothing -> throwError e
    Just a -> return a
