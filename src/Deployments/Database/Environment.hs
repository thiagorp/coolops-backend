module Deployments.Database.Environment
  ( module Common.PersistDatabase
  , getEnvironment
  , getEnvironmentBySlug
  , listEnvironments
  , listProjectEnvironments
  , listEnvironmentsForProjects
  ) where

import RIO hiding ((^.), on)

import Database.Esqueleto hiding (selectFirst)

import Common.PersistDatabase
import Model

getEnvironment :: (MonadIO m) => CompanyId -> UUID -> Db m (Maybe (Entity Environment))
getEnvironment companyId environmentId =
  selectFirst $
  from $ \(e `InnerJoin` p) -> do
    on ((p ^. ProjectId) ==. (e ^. EnvironmentProjectId))
    where_ (p ^. ProjectCompanyId ==. val companyId)
    where_ (e ^. EnvironmentId ==. val (EnvironmentKey environmentId))
    return e

getEnvironmentBySlug :: (MonadIO m) => CompanyId -> ProjectId -> Slug -> Db m (Maybe (Entity Environment))
getEnvironmentBySlug companyId pId slug =
  selectFirst $
  from $ \(e `InnerJoin` p) -> do
    on ((p ^. ProjectId) ==. (e ^. EnvironmentProjectId))
    where_ (p ^. ProjectCompanyId ==. val companyId)
    where_ (e ^. EnvironmentProjectId ==. val pId)
    where_ (e ^. EnvironmentSlug ==. val slug)
    return e

listEnvironments :: (MonadIO m) => CompanyId -> Db m [Entity Environment]
listEnvironments companyId =
  select $
  from $ \(e `InnerJoin` p) -> do
    on ((p ^. ProjectId) ==. (e ^. EnvironmentProjectId))
    where_ (p ^. ProjectCompanyId ==. val companyId)
    return e

listProjectEnvironments :: (MonadIO m) => CompanyId -> UUID -> Db m [Entity Environment]
listProjectEnvironments companyId projectId =
  select $
  from $ \(e `InnerJoin` p) -> do
    on ((p ^. ProjectId) ==. (e ^. EnvironmentProjectId))
    where_ (p ^. ProjectCompanyId ==. val companyId)
    where_ (p ^. ProjectId ==. val (ProjectKey projectId))
    return e

listEnvironmentsForProjects :: (MonadIO m) => CompanyId -> [ProjectId] -> Db m [Entity Environment]
listEnvironmentsForProjects _ [] = return []
listEnvironmentsForProjects companyId projectIds =
  select $
  from $ \(e `InnerJoin` p) -> do
    on ((p ^. ProjectId) ==. (e ^. EnvironmentProjectId))
    where_ (p ^. ProjectCompanyId ==. val companyId)
    where_ (p ^. ProjectId `in_` valList projectIds)
    return e
