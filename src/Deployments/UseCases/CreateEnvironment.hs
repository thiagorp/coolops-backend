module Deployments.UseCases.CreateEnvironment
  ( module Model
  , Params(..)
  , Error(..)
  , call
  ) where

import RIO

import Deployments.Database.Environment
import Deployments.Database.Project (getProject)
import Model

data Params = Params
  { paramName :: !EnvironmentName
  , paramEnvVars :: !(HashMap Text Text)
  , paramSlug :: !Slug
  }

data Error
  = ProjectNotFound
  | SlugAlreadyExists

build :: (MonadIO m) => ProjectId -> Params -> m Environment
build environmentProjectId Params {..} = do
  now <- liftIO getCurrentTime
  let environmentName = paramName
  let environmentEnvVars = paramEnvVars
  let environmentLockOnDeployment = False
  let environmentSlug = paramSlug
  let environmentCreatedAt = now
  let environmentUpdatedAt = now
  return Environment {..}

call_ :: (MonadIO m, HasDb m) => Entity Project -> Params -> Db m (Either Error (Entity Environment))
call_ (Entity pId Project {..}) params@Params {..} = do
  maybeEnvironment <- getEnvironmentBySlug projectCompanyId pId paramSlug
  case maybeEnvironment of
    Just _ -> return (Left SlugAlreadyExists)
    Nothing -> do
      environment <- build pId params
      envId <- insert environment
      return $ Right (Entity envId environment)

call :: (MonadIO m, HasDb m) => UUID -> CompanyId -> Params -> m (Either Error (Entity Environment))
call pId cId params =
  runDb $ do
    maybeProject <- getProject cId pId
    case maybeProject of
      Nothing -> return (Left ProjectNotFound)
      Just project -> call_ project params
