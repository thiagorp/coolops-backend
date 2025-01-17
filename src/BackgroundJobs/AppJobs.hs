{-# LANGUAGE OverloadedStrings #-}

module BackgroundJobs.AppJobs
  ( notifySlackDeployer
  , notifyDeploymentQueuedByLock
  , notifyNewEnvironmentLock
  , notifyBuild
  , runNext
  ) where

import Import

import Data.Aeson

import qualified BackgroundJobs.Handlers.NotifyBuild as NotifyBuild
import qualified BackgroundJobs.Handlers.NotifyDeploymentQueuedByLock as NotifyDeploymentQueuedByLock
import qualified BackgroundJobs.Handlers.NotifyNewEnvironmentLock as NotifyNewEnvironmentLock
import qualified BackgroundJobs.Handlers.NotifySlackDeployer as NotifySlackDeployer
import qualified BackgroundJobs.Runner as Runner

data Job
  = NotifyBuild NotifyBuild.Params
  | NotifyDeploymentQueuedByLock NotifyDeploymentQueuedByLock.Params
  | NotifyNewEnvironmentLock NotifyNewEnvironmentLock.Params
  | NotifySlackDeployer NotifySlackDeployer.Params

encode' :: FromJSON a => Value -> Maybe a
encode' value =
  case fromJSON value of
    Error _ -> Nothing
    Success a -> Just a

deserialize :: Text -> Value -> Maybe Job
deserialize jobName value =
  case jobName of
    "NotifyBuild" -> NotifyBuild <$> encode' value
    "NotifyDeploymentQueuedByLock" -> NotifyDeploymentQueuedByLock <$> encode' value
    "NotifyNewEnvironmentLock" -> NotifyNewEnvironmentLock <$> encode' value
    "NotifySlackDeployer" -> NotifySlackDeployer <$> encode' value
    _ -> Nothing

serialize :: Job -> (Text, Value)
serialize job =
  case job of
    NotifyBuild params -> ("NotifyBuild", toJSON params)
    NotifyDeploymentQueuedByLock params -> ("NotifyDeploymentQueuedByLock", toJSON params)
    NotifyNewEnvironmentLock params -> ("NotifyNewEnvironmentLock", toJSON params)
    NotifySlackDeployer params -> ("NotifySlackDeployer", toJSON params)

run :: Int -> Job -> App Runner.JobReturnType
run _ job =
  case job of
    NotifyBuild params -> NotifyBuild.call params
    NotifyDeploymentQueuedByLock params -> NotifyDeploymentQueuedByLock.call params
    NotifyNewEnvironmentLock params -> NotifyNewEnvironmentLock.call params
    NotifySlackDeployer params -> NotifySlackDeployer.call params

config :: Runner.JobConfig Job
config = Runner.JobConfig deserialize serialize run

queue :: Job -> App BackgroundJobId
queue = Runner.queue config

runNext :: App ()
runNext = Runner.runNext config

notifyBuild :: CompanyId -> UUID -> App BackgroundJobId
notifyBuild cId bId = queue . NotifyBuild $ NotifyBuild.Params cId bId

notifyDeploymentQueuedByLock :: DeploymentId -> EnvironmentLockId -> Text -> App BackgroundJobId
notifyDeploymentQueuedByLock dId elId tuId =
  queue . NotifyDeploymentQueuedByLock $
    NotifyDeploymentQueuedByLock.Params dId elId tuId

notifyNewEnvironmentLock :: CompanyId -> EnvironmentLockId -> App BackgroundJobId
notifyNewEnvironmentLock cId lId = queue . NotifyNewEnvironmentLock $ NotifyNewEnvironmentLock.Params cId lId

notifySlackDeployer :: CompanyId -> UUID -> App BackgroundJobId
notifySlackDeployer cId bId = queue . NotifySlackDeployer $ NotifySlackDeployer.Params cId bId
