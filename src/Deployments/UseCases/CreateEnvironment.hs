module Deployments.UseCases.CreateEnvironment
  ( Params(..)
  , Error(..)
  , call
  ) where

import RIO

import Common.Database (HasPostgres)
import Deployments.Database.Environment (createEnvironment, getEnvironmentBySlug)
import Deployments.Database.Project (getProject)
import qualified Deployments.Domain.Environment as Environment
import qualified Deployments.Domain.Project as Project

data Params = Params
  { environmentName :: !Environment.Name
  , environmentEnvVars :: !(HashMap Text Text)
  , environmentSlug :: !Environment.Slug
  }

data Error
  = ProjectNotFound
  | SlugAlreadyExists

build :: (MonadIO m) => Environment.ProjectID -> Params -> m Environment.Environment
build environmentProjectId Params {..} = do
  environmentId <- Environment.genId
  return Environment.Environment {..}

call_ :: (MonadIO m, HasPostgres m) => Project.Project -> Params -> m (Either Error Environment.Environment)
call_ Project.Project {..} params = do
  maybeEnvironment <- getEnvironmentBySlug projectCompanyId projectId (environmentSlug params)
  case maybeEnvironment of
    Just _ -> return (Left SlugAlreadyExists)
    Nothing -> do
      environment <- build projectId params
      createEnvironment environment
      return (Right environment)

call :: (MonadIO m, HasPostgres m) => Text -> Project.CompanyID -> Params -> m (Either Error Environment.Environment)
call pId cId params = do
  maybeProject <- getProject cId pId
  case maybeProject of
    Nothing -> return (Left ProjectNotFound)
    Just project -> call_ project params
