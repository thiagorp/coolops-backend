module Deployments.Classes where

import RIO

import Deployments.Database.Project (DBProjectID)
import Deployments.Domain.Build (Build)
import Deployments.Domain.Deployment
  ( DeploymentResources
  , FinishedDeployment
  , QueuedDeployment
  , RunningDeployment
  )
import Deployments.Domain.Environment (Environment)
import Deployments.Domain.Project (CompanyID, Project)

class (Monad m) =>
      ProjectRepo m
  where
  createProject :: Project -> m ()
  updateProject :: Project -> m ()
  getProject :: (DBProjectID a) => CompanyID -> a -> m (Maybe Project)
  getProjectForBuild :: Build -> m (Maybe Project)
  listProjects :: CompanyID -> m [Project]
  findProjectByAccessToken :: Text -> m (Maybe Project)

class (Monad m) =>
      EnvironmentRepo m
  where
  createEnvironment :: Environment -> m ()
  getEnvironment :: CompanyID -> Text -> m (Maybe Environment)
  listEnvironments :: CompanyID -> m [Environment]
  listProjectEnvironments :: CompanyID -> Text -> m [Environment]
  updateEnvironment :: Environment -> m ()

class (Monad m) =>
      BuildRepo m
  where
  createBuild :: Build -> m ()
  getBuild :: CompanyID -> Text -> m (Maybe Build)

class (Monad m) =>
      DeploymentRepo m
  where
  createQueuedDeployment :: QueuedDeployment -> m ()
  getNextQueuedDeployment :: CompanyID -> m (Maybe QueuedDeployment)
  saveRunningDeployment :: RunningDeployment -> m ()
  saveFinishedDeployment :: FinishedDeployment -> m ()
  getDeploymentResources ::
       CompanyID -> Text -> Text -> m (Maybe DeploymentResources)
