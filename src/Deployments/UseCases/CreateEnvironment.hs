module Deployments.UseCases.CreateEnvironment
  ( Params(..)
  , Error(..)
  , EnvironmentRepo
  , call
  , call_
  ) where

import RIO

import Deployments.Classes
import qualified Deployments.Domain.Environment as Environment
import qualified Deployments.Domain.Project as Project

data Params = Params
  { environmentName :: !Environment.Name
  , environmentEnvVars :: !(HashMap Text Text)
  }

data Error =
  ProjectNotFound

build ::
     (MonadIO m) => Environment.ProjectID -> Params -> m Environment.Environment
build environmentProjectId Params {..} = do
  environmentId <- Environment.genId
  return Environment.Environment {..}

call_ ::
     (MonadIO m, EnvironmentRepo m)
  => Project.Project
  -> Params
  -> m Environment.Environment
call_ Project.Project {..} params = do
  environment <- build projectId params
  createEnvironment environment
  return environment

call ::
     (MonadIO m, EnvironmentRepo m, ProjectRepo m)
  => Text
  -> Project.CompanyID
  -> Params
  -> m (Either Error Environment.Environment)
call pId cId params = do
  maybeProject <- getProject cId pId
  case maybeProject of
    Nothing -> return (Left ProjectNotFound)
    Just project -> Right <$> call_ project params
