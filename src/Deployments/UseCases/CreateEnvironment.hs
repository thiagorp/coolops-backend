module Deployments.UseCases.CreateEnvironment
  ( Params(..)
  , Error(..)
  , call
  ) where

import RIO

import Deployments.Classes
import Deployments.Domain

data Params = Params
  { environmentName :: !EnvironmentName
  , environmentEnvVars :: !(HashMap Text Text)
  , pId :: !Text
  , cId :: !CompanyID
  }

data Error =
  ProjectNotFound

build :: (MonadIO m) => ProjectID -> Params -> m Environment
build environmentProjectId Params {..} = do
  environmentId <- genEnvironmentId
  return Environment {..}

create :: (EnvironmentRepo m, MonadIO m) => ProjectID -> Params -> m Environment
create projectId params = do
  environment <- build projectId params
  createEnvironment environment
  return environment

call ::
     (MonadIO m, EnvironmentRepo m, ProjectRepo m)
  => Params
  -> m (Either Error Environment)
call params = do
  maybeProject <- getProject (cId params) (pId params)
  case maybeProject of
    Nothing -> return (Left ProjectNotFound)
    Just project -> Right <$> create (projectId project) params
