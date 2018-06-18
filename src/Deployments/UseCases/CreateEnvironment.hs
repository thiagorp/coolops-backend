module Deployments.UseCases.CreateEnvironment
  ( Params(..)
  , Error(..)
  , call
  ) where

import RIO

import Deployments.Classes
import qualified Deployments.Domain.Environment as Environment
import qualified Deployments.Domain.Project as Project

data Params = Params
  { environmentName :: !Environment.Name
  , environmentEnvVars :: !(HashMap Text Text)
  , pId :: !Text
  , cId :: !Project.CompanyID
  }

data Error =
  ProjectNotFound

build ::
     (MonadIO m) => Environment.ProjectID -> Params -> m Environment.Environment
build environmentProjectId Params {..} = do
  environmentId <- Environment.genId
  return Environment.Environment {..}

create ::
     (EnvironmentRepo m, MonadIO m)
  => Environment.ProjectID
  -> Params
  -> m Environment.Environment
create projectId params = do
  environment <- build projectId params
  createEnvironment environment
  return environment

call ::
     (MonadIO m, EnvironmentRepo m, ProjectRepo m)
  => Params
  -> m (Either Error Environment.Environment)
call params = do
  maybeProject <- getProject (cId params) (pId params)
  case maybeProject of
    Nothing -> return (Left ProjectNotFound)
    Just project -> Right <$> create (Project.projectId project) params
