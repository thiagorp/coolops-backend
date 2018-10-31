module Deployments.UseCases.CreateProject
  ( module Model
  , module Deployments.Database.Project
  , Params(..)
  , Error(..)
  , call
  ) where

import RIO

import Deployments.Database.Project
import Model

data Error =
  SlugAlreadyExists

data Params = Params
  { paramName :: !ProjectName
  , paramSlug :: !Slug
  , paramDeploymentImage :: !DockerImage
  , paramCompanyId :: !CompanyId
  }

build :: (MonadIO m) => Params -> Db m Project
build Params {..} = do
  projectAccessToken <- genAccessToken
  now <- liftIO getCurrentTime
  let projectName = paramName
  let projectCompanyId = paramCompanyId
  let projectSlug = paramSlug
  let projectDeploymentImage = paramDeploymentImage
  let projectCreatedAt = now
  let projectUpdatedAt = now
  return Project {..}

call :: (MonadIO m, HasDb m) => Params -> Db m (Either Error (Entity Project))
call params@Params {..} = do
  maybeProject <- getProjectBySlug paramCompanyId paramSlug
  case maybeProject of
    Just _ -> return (Left SlugAlreadyExists)
    Nothing -> do
      project <- build params
      projectId <- insert project
      return (Right (Entity projectId project))
