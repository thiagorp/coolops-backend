module Deployments.UseCases.CreateProject
  ( Params(..)
  , ProjectName
  , ProjectSlug
  , Project.Project
  , Error(..)
  , call
  ) where

import RIO

import Common.Database (HasPostgres)
import Deployments.Database.Project (createProject, getProjectBySlug)
import qualified Deployments.Domain.Project as Project

data Error =
  SlugAlreadyExists

type ProjectName = Project.Name

type ProjectSlug = Project.Slug

data Params = Params
  { projectName :: !ProjectName
  , projectSlug :: !Project.Slug
  , projectDeploymentImage :: !Project.DeploymentImage
  , companyId :: !Project.CompanyID
  }

build :: (MonadIO m) => Params -> m Project.Project
build Params {..} = do
  projectId <- Project.genId
  projectAccessToken <- Project.genAccessToken
  let projectCompanyId = companyId
  return Project.Project {..}

call :: (MonadIO m, HasPostgres m) => Params -> m (Either Error Project.Project)
call params = do
  maybeProject <- getProjectBySlug (companyId params) (projectSlug params)
  case maybeProject of
    Just _ -> return (Left SlugAlreadyExists)
    Nothing -> do
      project <- build params
      createProject project
      return (Right project)
