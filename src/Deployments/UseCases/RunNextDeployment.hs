module Deployments.UseCases.RunNextDeployment
  ( Error(..)
  , CallMonad
  , call
  ) where

import RIO

import Control.Monad.Except

import Common.Database
import Deployments.Classes
import qualified Deployments.Domain.Build as B
import Deployments.Domain.Deployment
import qualified Deployments.Domain.Environment as E
import qualified Deployments.Domain.Project as P
import Deployments.Gateway.Kubernetes
import Util.Key

data Error
  = NoDeploymentToRun
  | MissingEntities
  | FailedToRunJob

type CallMonad m
   = ( HasDBTransaction m
     , DeploymentRepo m
     , RunDeploymentMonad m
     , ProjectRepo m
     , BuildRepo m
     , EnvironmentRepo m)

type RunMonad m a = ExceptT Error m a

runNext ::
     (DeploymentRepo m, RunDeploymentMonad m)
  => QueuedDeployment
  -> E.Environment
  -> B.Build
  -> P.Project
  -> RunMonad m RunningDeployment
runNext queued environment build project = do
  running <- run queued
  lift $ saveRunningDeployment running
  result <- lift $ runDeployment queued environment build project
  case result of
    True -> return running
    False -> throwError FailedToRunJob

getEnvironment_ ::
     EnvironmentRepo m => P.CompanyID -> E.ID -> RunMonad m E.Environment
getEnvironment_ cId eId =
  handleEntity MissingEntities $ getEnvironment cId $ keyText eId

getBuild_ :: BuildRepo m => P.CompanyID -> B.ID -> RunMonad m B.Build
getBuild_ cId bId = handleEntity MissingEntities $ getBuild cId $ keyText bId

getProject_ :: ProjectRepo m => P.CompanyID -> P.ID -> RunMonad m P.Project
getProject_ cId pId =
  handleEntity MissingEntities $ getProject cId $ keyText pId

getNextQueuedDeployment_ ::
     DeploymentRepo m => P.CompanyID -> RunMonad m QueuedDeployment
getNextQueuedDeployment_ cId =
  handleEntity NoDeploymentToRun $ getNextQueuedDeployment cId

call_ :: CallMonad m => P.CompanyID -> RunMonad m RunningDeployment
call_ companyId = do
  deployment <- getNextQueuedDeployment_ companyId
  environment <- getEnvironment_ companyId (deploymentEnvironmentId deployment)
  build <- getBuild_ companyId (deploymentBuildId deployment)
  project <- getProject_ companyId (B.buildProjectId build)
  runNext deployment environment build project

call :: CallMonad m => P.CompanyID -> m (Either Error RunningDeployment)
call = runExceptT . call_

handleEntity :: (Monad m) => Error -> m (Maybe a) -> RunMonad m a
handleEntity e wrappedEntity = do
  entity <- lift wrappedEntity
  case entity of
    Nothing -> throwError e
    Just a -> return a
