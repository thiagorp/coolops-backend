module Deployments.Classes where

import RIO

import Deployments.Domain.Build
import Deployments.Domain.Environment
import Deployments.Domain.Project

class (Monad m) =>
      ProjectRepo m
  where
  createProject :: Project -> m ()
  updateProject :: Project -> m ()
  getProject :: CompanyID -> Text -> m (Maybe Project)
  listProjects :: CompanyID -> m [Project]
  findProjectByAccessToken :: Text -> m (Maybe Project)

class (Monad m) =>
      EnvironmentRepo m
  where
  createEnvironment :: Environment -> m ()

class (Monad m) =>
      BuildRepo m
  where
  createBuild :: Build -> m ()
