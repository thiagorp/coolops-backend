module Deployments.Classes where

import RIO

import Deployments.Domain.Build (Build)
import Deployments.Domain.Deployment (Deployment)
import Deployments.Domain.Environment (Environment)
import Deployments.Domain.Project (CompanyID, Project)

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
  getEnvironment :: CompanyID -> Text -> m (Maybe Environment)

class (Monad m) =>
      BuildRepo m
  where
  createBuild :: Build -> m ()
  getBuild :: CompanyID -> Text -> m (Maybe Build)

class (Monad m) =>
      DeploymentRepo m
  where
  createDeployment :: Deployment -> m ()
