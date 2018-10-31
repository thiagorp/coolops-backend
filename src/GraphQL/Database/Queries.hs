module GraphQL.Database.Queries
  ( module Model
  , module Common.PersistDatabase
  , getCompany
  , getSlackAccessToken
  , listBuilds
  , listBuildsById
  , listEnvironments
  , listEnvironmentsById
  , listEnvsLastDeployments
  , listProjects
  , listProjectsById
  , listSlackProjectIntegrations
  , listUsersById
  ) where

import RIO hiding ((^.), on)

import Database.Esqueleto hiding (selectFirst)

import Common.PersistDatabase
import Model

getCompany :: (MonadIO m) => CompanyId -> Db m (Maybe (Entity Company))
getCompany companyId =
  selectFirst $
  from $ \c -> do
    where_ (c ^. CompanyId ==. val companyId)
    return c

getSlackAccessToken :: (MonadIO m) => CompanyId -> Db m (Maybe (Entity SlackAccessToken))
getSlackAccessToken companyId =
  selectFirst $
  from $ \a -> do
    where_ (a ^. SlackAccessTokenCompanyId ==. val companyId)
    return a

listBuilds :: (MonadIO m) => (Int64, Int64) -> Maybe ProjectId -> CompanyId -> Db m [Entity Build]
listBuilds (l, o) maybeProjectId companyId =
  select $
  from $ \(b `InnerJoin` p) -> do
    on ((p ^. ProjectId) ==. (b ^. BuildProjectId))
    where_ (p ^. ProjectCompanyId ==. val companyId)
    case maybeProjectId of
      Nothing -> pure ()
      Just pId -> where_ (b ^. BuildProjectId ==. val pId)
    orderBy [desc (b ^. BuildCreatedAt)]
    limit l
    offset o
    return b

listBuildsById :: (MonadIO m) => CompanyId -> [BuildId] -> Db m [Entity Build]
listBuildsById _ [] = return []
listBuildsById companyId ids =
  select $
  from $ \(b `InnerJoin` p) -> do
    on ((p ^. ProjectId) ==. (b ^. BuildProjectId))
    where_ (p ^. ProjectCompanyId ==. val companyId)
    where_ (b ^. BuildId `in_` valList ids)
    return b

listProjects :: (MonadIO m) => CompanyId -> Db m [Entity Project]
listProjects companyId =
  select $
  from $ \p -> do
    where_ (p ^. ProjectCompanyId ==. val companyId)
    orderBy [asc (p ^. ProjectName)]
    return p

listProjectsById :: (MonadIO m) => CompanyId -> [ProjectId] -> Db m [Entity Project]
listProjectsById companyId ids =
  select $
  from $ \p -> do
    where_ (p ^. ProjectCompanyId ==. val companyId)
    where_ (p ^. ProjectId `in_` valList ids)
    return p

listEnvironments :: (MonadIO m) => CompanyId -> [ProjectId] -> Db m [Entity Environment]
listEnvironments companyId projectIds =
  select $
  from $ \(e `InnerJoin` p) -> do
    on ((p ^. ProjectId) ==. (e ^. EnvironmentProjectId))
    where_ (p ^. ProjectCompanyId ==. val companyId)
    where_ (p ^. ProjectId `in_` valList projectIds)
    orderBy [asc (e ^. EnvironmentName)]
    return e

listEnvironmentsById :: (MonadIO m) => CompanyId -> [EnvironmentId] -> Db m [Entity Environment]
listEnvironmentsById companyId ids =
  select $
  from $ \(e `InnerJoin` p) -> do
    on ((p ^. ProjectId) ==. (e ^. EnvironmentProjectId))
    where_ (p ^. ProjectCompanyId ==. val companyId)
    where_ (e ^. EnvironmentId `in_` valList ids)
    return e

listEnvsLastDeployments :: (MonadIO m) => CompanyId -> [EnvironmentId] -> Db m [Entity Deployment]
listEnvsLastDeployments companyId envIds =
  select $
  from $ \(d `InnerJoin` e `InnerJoin` p) -> do
    on ((p ^. ProjectId) ==. (e ^. EnvironmentProjectId))
    on ((e ^. EnvironmentId) ==. (d ^. DeploymentEnvironmentId))
    where_ (p ^. ProjectCompanyId ==. val companyId)
    where_ (e ^. EnvironmentId `in_` valList envIds)
    orderBy [desc (p ^. ProjectId), desc (e ^. EnvironmentId), desc (d ^. DeploymentCreatedAt)]
    return d

listSlackProjectIntegrations :: (MonadIO m) => CompanyId -> [ProjectId] -> Db m [Entity SlackProjectIntegration]
listSlackProjectIntegrations companyId projectIds =
  select $
  from $ \(spi `InnerJoin` p) -> do
    on ((p ^. ProjectId) ==. (spi ^. SlackProjectIntegrationProjectId))
    where_ (p ^. ProjectCompanyId ==. val companyId)
    where_ (p ^. ProjectId `in_` valList projectIds)
    return spi

listUsersById :: (MonadIO m) => CompanyId -> [UserId] -> Db m [Entity User]
listUsersById companyId ids =
  select $
  from $ \u -> do
    where_ (u ^. UserCompanyId ==. val companyId)
    where_ (u ^. UserId `in_` valList ids)
    return u
