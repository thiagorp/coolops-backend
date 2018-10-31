module Deployments.Database.Deployment
  ( module Common.PersistDatabase
  , module Model
  , getDeployment
  , getDeploymentResources
  , getDeploymentsResources
  , getNextQueuedDeployment
  , listAllRunningDeployments
  ) where

import RIO hiding ((^.), on)

import Database.Esqueleto hiding (selectFirst)

import Common.PersistDatabase
import Deployments.Database.Build
import Deployments.Database.Environment
import Deployments.Domain.Deployment
import Model

getNextQueuedDeployment :: (MonadIO m) => CompanyId -> Db m (Maybe (Entity Deployment))
getNextQueuedDeployment companyId =
  selectFirst $
  from $ \(d `InnerJoin` e `InnerJoin` p) -> do
    on ((p ^. ProjectId) ==. (e ^. EnvironmentProjectId))
    on ((e ^. EnvironmentId) ==. (d ^. DeploymentEnvironmentId))
    where_ (p ^. ProjectCompanyId ==. val companyId)
    where_ (d ^. DeploymentStatus ==. val Queued)
    orderBy [asc (d ^. DeploymentCreatedAt)]
    locking ForUpdateSkipLocked
    return d

getDeploymentsResources :: (MonadIO m) => Entity Deployment -> Db m (Maybe DeploymentResources)
getDeploymentsResources (Entity _ Deployment {..}) = do
  maybeEnvironment <- get deploymentEnvironmentId
  maybeBuild <- get deploymentBuildId
  case (,) <$> maybeEnvironment <*> maybeBuild of
    Nothing -> return Nothing
    Just (environment, build@Build {..}) -> do
      maybeProject <- get buildProjectId
      case maybeProject of
        Nothing -> return Nothing
        Just project ->
          return $
          Just $
          DeploymentResources
            (Entity buildProjectId project)
            (Entity deploymentEnvironmentId environment)
            (Entity deploymentBuildId build)

getDeploymentResources :: (MonadIO m) => CompanyId -> UUID -> UUID -> Db m (Maybe DeploymentResources)
getDeploymentResources cId eId bId = do
  maybeEnvironment <- getEnvironment cId eId
  maybeBuild <- getBuild cId bId
  case (,) <$> maybeEnvironment <*> maybeBuild of
    Nothing -> return Nothing
    Just (environment, build@(Entity _ Build {..})) -> do
      maybeProject <- get buildProjectId
      case maybeProject of
        Nothing -> return Nothing
        Just project -> return $ Just $ DeploymentResources (Entity buildProjectId project) environment build

listAllRunningDeployments :: (MonadIO m) => Db m [(CompanyId, Entity Deployment)]
listAllRunningDeployments = do
  values <-
    select $
    from $ \(d `InnerJoin` e `InnerJoin` p) -> do
      on ((p ^. ProjectId) ==. (e ^. EnvironmentProjectId))
      on ((e ^. EnvironmentId) ==. (d ^. DeploymentEnvironmentId))
      where_ (d ^. DeploymentStatus ==. val Running)
      return (p ^. ProjectCompanyId, d)
  return $ map (\(v, d) -> (unValue v, d)) values

getDeployment :: (MonadIO m) => UUID -> Db m (Maybe (Entity Deployment))
getDeployment dId =
  selectFirst $
  from $ \d -> do
    where_ (d ^. DeploymentId ==. val (DeploymentKey dId))
    return d
