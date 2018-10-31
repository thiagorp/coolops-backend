module Deployments.UseCases.FinishDeployment
  ( call
  ) where

import RIO

import qualified BackgroundJobs.AppJobs as Background
import Common.PersistDatabase
import Model

notifyDeployer :: (Background.NotifySlackDeployerConstraint m) => CompanyId -> Entity Deployment -> Db m ()
notifyDeployer cId (Entity (DeploymentKey deploymentKey) _) = void $ Background.notifySlackDeployer cId deploymentKey

updateBuildMessage :: (Background.NotifyBuildConstraint m) => CompanyId -> Entity Deployment -> Db m ()
updateBuildMessage cId (Entity _ Deployment {..}) = void $ Background.notifyBuild cId buildKey
  where
    BuildKey buildKey = deploymentBuildId

call ::
     (MonadIO m, Background.NotifyBuildConstraint m, Background.NotifySlackDeployerConstraint m)
  => DeploymentStatus
  -> CompanyId
  -> Entity Deployment
  -> Db m ()
call status companyId deployment = do
  now <- liftIO getCurrentTime
  update
    (entityKey deployment)
    [DeploymentFinishedAt =. Just now, DeploymentStatus =. status, DeploymentUpdatedAt =. now]
  notifyDeployer companyId deployment
  updateBuildMessage companyId deployment
