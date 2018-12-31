module Deployments.UseCases.FinishDeployment
  ( call
  ) where

import Import

import qualified BackgroundJobs.AppJobs as Background

notifyDeployer :: CompanyId -> Entity Deployment -> App ()
notifyDeployer cId (Entity (DeploymentKey deploymentKey) _) = void $ Background.notifySlackDeployer cId deploymentKey

updateBuildMessage :: CompanyId -> Entity Deployment -> App ()
updateBuildMessage cId (Entity _ Deployment {..}) = void $ Background.notifyBuild cId buildKey
  where
    BuildKey buildKey = deploymentBuildId

call :: DeploymentStatus -> CompanyId -> Entity Deployment -> App ()
call status companyId deployment = do
  now <- liftIO getCurrentTime
  update
    (entityKey deployment)
    [DeploymentFinishedAt =. Just now, DeploymentStatus =. status, DeploymentUpdatedAt =. now]
  notifyDeployer companyId deployment
  updateBuildMessage companyId deployment
