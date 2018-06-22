module Deployments.UseCases.RunNextDeployment
  ( Error(..)
  , call
  ) where

import RIO

import Common.Database
import Deployments.Classes
import Deployments.Domain.Build (Build)
import Deployments.Domain.Deployment
import Deployments.Domain.Environment (Environment, environmentProjectId)
import Deployments.Domain.Project (CompanyID, Project)
import Deployments.Gateway.Kubernetes
import Util.Key

data Error
  = NoDeploymentToRun
  | MissingEntities
  | FailedToRunJob

runNext ::
     (DeploymentRepo m, RunDeploymentMonad m)
  => QueuedDeployment
  -> Environment
  -> Build
  -> Project
  -> m (Either Error RunningDeployment)
runNext queued environment build project = do
  running <- run queued
  saveRunningDeployment running
  result <- runDeployment queued environment build project
  case result of
    True -> return $ Right running
    False -> return $ Left FailedToRunJob

fetchEntities ::
     (ProjectRepo m, BuildRepo m, EnvironmentRepo m)
  => CompanyID
  -> QueuedDeployment
  -> m (Maybe (Environment, Build, Project))
fetchEntities companyId QueuedDeployment {..} = do
  maybeEnvironment <- getEnvironment companyId (keyText deploymentEnvironmentId)
  maybeBuild <- getBuild companyId (keyText deploymentBuildId)
  case (,) <$> maybeEnvironment <*> maybeBuild of
    Nothing -> return Nothing
    Just (environment, build) -> do
      maybeProject <-
        getProject companyId (keyText (environmentProjectId environment))
      return $ (environment, build, ) <$> maybeProject

call ::
     ( HasDBTransaction m
     , DeploymentRepo m
     , RunDeploymentMonad m
     , ProjectRepo m
     , BuildRepo m
     , EnvironmentRepo m
     )
  => CompanyID
  -> m (Either Error RunningDeployment)
call companyId =
  runEitherTransaction $ do
    maybeDeployment <- getNextQueuedDeployment companyId
    case maybeDeployment of
      Nothing -> return $ Left NoDeploymentToRun
      Just deployment -> do
        maybeEntities <- fetchEntities companyId deployment
        case maybeEntities of
          Nothing -> return $ Left MissingEntities
          Just (environment, build, project) ->
            runNext deployment environment build project
