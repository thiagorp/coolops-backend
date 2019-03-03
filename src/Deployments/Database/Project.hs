{-# LANGUAGE RecordWildCards #-}

module Deployments.Database.Project
  ( findProjectByAccessToken
  , getProject
  , getProjectBySlug
  , getProjectForBuild
  , listProjects
  ) where

import RIO hiding ((^.), on)

import Database.Esqueleto hiding (selectFirst)

import Common.PersistDatabase
import Model

listProjects :: (MonadIO m) => CompanyId -> Db m [Entity Project]
listProjects companyId =
  select $
  from $ \p -> do
    where_ (p ^. ProjectCompanyId ==. val companyId)
    return p

getProject :: (MonadIO m) => CompanyId -> UUID -> Db m (Maybe (Entity Project))
getProject companyId projectId =
  selectFirst $
  from $ \p -> do
    where_ (p ^. ProjectCompanyId ==. val companyId)
    where_ (p ^. ProjectId ==. val (ProjectKey projectId))
    return p

getProjectBySlug :: (MonadIO m) => CompanyId -> Slug -> Db m (Maybe (Entity Project))
getProjectBySlug companyId slug =
  selectFirst $
  from $ \p -> do
    where_ (p ^. ProjectCompanyId ==. val companyId)
    where_ (p ^. ProjectSlug ==. val slug)
    return p

getProjectForBuild :: (MonadIO m) => Build -> Db m (Maybe (Entity Project))
getProjectForBuild Build {..} =
  selectFirst $
  from $ \p -> do
    where_ (p ^. ProjectId ==. val buildProjectId)
    return p

findProjectByAccessToken :: (MonadIO m) => AccessToken -> Db m (Maybe (Entity Project))
findProjectByAccessToken accessToken =
  selectFirst $
  from $ \p -> do
    where_ (p ^. ProjectAccessToken ==. val accessToken)
    return p
