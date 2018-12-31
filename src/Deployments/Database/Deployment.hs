module Deployments.Database.Deployment
  ( getDeployment
  , getDeploymentResources
  , getDeploymentsResources
  , getNextQueuedDeployment
  , listAllRunningDeployments
  ) where

import RIO hiding ((^.), isNothing, on)

import Database.Esqueleto hiding (selectFirst)

import Common.PersistDatabase
import Deployments.Domain.Deployment

getNextQueuedDeployment :: (MonadIO m) => CompanyId -> Db m (Maybe (Entity Deployment))
getNextQueuedDeployment companyId =
  selectFirst $ fromDeployments companyId $ \d e -> do
    where_ (d ^. DeploymentStatus ==. val Queued)
    where_ (e ^. EnvironmentId `notIn` subList_select (distinct runningEnvironmentIds))
    where_ (e ^. EnvironmentId `notIn` subList_select (distinct lockedEnvironmentIds))
    orderBy [asc (d ^. DeploymentCreatedAt)]
    locking ForUpdateSkipLocked
    return d

  where
    lockedEnvironmentIds =
      from $ \(e `InnerJoin` p) -> do
        on $ (p ^. ProjectId) ==. (e ^. EnvironmentProjectId)
        where_ $ p ^. ProjectCompanyId ==. val companyId
        where_ $ exists $
          from $ \l -> do
            where_ $ (l ^. EnvironmentLockEnvironmentId) ==. (e ^. EnvironmentId)
            where_ $ isNothing $ l ^. EnvironmentLockReleasedAt
        return $ e ^. EnvironmentId

    runningEnvironmentIds = fromDeployments companyId $ \d e -> do
      where_ $ d ^. DeploymentStatus ==. val Running
      return $ e ^. EnvironmentId

    fromDeployments cId query =
      from $ \(d `InnerJoin` e `InnerJoin` p) -> do
        on ((p ^. ProjectId) ==. (e ^. EnvironmentProjectId))
        on ((e ^. EnvironmentId) ==. (d ^. DeploymentEnvironmentId))
        where_ (p ^. ProjectCompanyId ==. val cId)
        query d e

getDeploymentsResources :: (MonadIO m) => Entity Deployment -> Db m (Maybe DeploymentResources)
getDeploymentsResources (Entity deploymentId _) =
  deploymentResourcesQuery $ \d _ ->
    where_ $ d ^. DeploymentId ==. val deploymentId

getDeploymentResources :: (MonadIO m) => CompanyId -> UUID -> UUID -> Db m (Maybe DeploymentResources)
getDeploymentResources cId eId bId =
  deploymentResourcesQuery $ \d p -> do
    where_ $ p ^. ProjectCompanyId ==. val cId
    where_ $ d ^. DeploymentEnvironmentId ==. val (EnvironmentKey eId)
    where_ $ d ^. DeploymentBuildId ==. val (BuildKey bId)

deploymentResourcesQuery ::
  (MonadIO m) =>
  (SqlExpr (Entity Deployment) -> SqlExpr (Entity Project) -> SqlQuery ()) ->
  Db m (Maybe DeploymentResources)
deploymentResourcesQuery condition = do
  maybeResources <-
    selectFirst $
      from $ \(d `InnerJoin` e `InnerJoin` p `InnerJoin` b `InnerJoin` sd) -> do
        on $ (sd ^. SlackDeploymentDeploymentId) ==. (d ^. DeploymentId)
        on $ (b ^. BuildProjectId) ==. (p ^. ProjectId)
        on $ (p ^. ProjectId) ==. (e ^. EnvironmentProjectId)
        on $ (e ^. EnvironmentId) ==. (d ^. DeploymentEnvironmentId)
        condition d p
        return (p, e, b, sd ^. SlackDeploymentSlackUserId)

  case maybeResources of
    Nothing ->
      return Nothing

    Just (project, environment, build, Value userId) ->
      return $ Just (DeploymentResources project environment build userId)

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
