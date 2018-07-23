module BackgroundJobs.AppJobs
  ( NotifyBuildConstraint
  , NotifySlackDeployerConstraint
  , notifySlackDeployer
  , notifyBuild
  , runNext
  ) where

import RIO

import Data.Aeson

import qualified BackgroundJobs.Handlers.NotifyBuild as NotifyBuild
import qualified BackgroundJobs.Handlers.NotifySlackDeployer as NotifySlackDeployer

import Auth.Domain (CompanyID)
import qualified BackgroundJobs.Runner as Runner

data Job
  = NotifyBuild NotifyBuild.Params
  | NotifySlackDeployer NotifySlackDeployer.Params

type NotifyBuildConstraint m = NotifyBuild.CallConstraint m

type NotifySlackDeployerConstraint m = NotifySlackDeployer.CallConstraint m

type JobMonad m
   = (NotifyBuild.CallConstraint m, NotifySlackDeployer.CallConstraint m)

encode' :: FromJSON a => Value -> Maybe a
encode' value =
  case fromJSON value of
    Error _ -> Nothing
    Success a -> Just a

deserialize :: Text -> Value -> Maybe Job
deserialize jobName value =
  case jobName of
    "NotifyBuild" -> NotifyBuild <$> encode' value
    "NotifySlackDeployer" -> NotifySlackDeployer <$> encode' value
    _ -> Nothing

serialize :: Job -> (Text, Value)
serialize job =
  case job of
    NotifyBuild params -> ("NotifyBuild", toJSON params)
    NotifySlackDeployer params -> ("NotifySlackDeployer", toJSON params)

run :: JobMonad m => Int -> Job -> m Runner.JobReturnType
run _ job =
  case job of
    NotifyBuild params -> NotifyBuild.call params
    NotifySlackDeployer params -> NotifySlackDeployer.call params

config :: JobMonad m => Runner.JobConfig m Job
config = Runner.JobConfig deserialize serialize run

queue :: JobMonad m => Job -> m ()
queue = Runner.queue config

runNext :: JobMonad m => m ()
runNext = Runner.runNext config

notifyBuild :: JobMonad m => CompanyID -> Text -> m ()
notifyBuild cId bId = queue . NotifyBuild $ NotifyBuild.Params cId bId

notifySlackDeployer :: JobMonad m => CompanyID -> Text -> m ()
notifySlackDeployer cId bId =
  queue . NotifySlackDeployer $ NotifySlackDeployer.Params cId bId
