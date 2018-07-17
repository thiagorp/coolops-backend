module BackgroundJobs.AppJobs
  ( NotifyBuildConstraint
  , notifyBuild
  , runNext
  ) where

import RIO

import Data.Aeson

import qualified BackgroundJobs.Handlers.NotifyBuild as NotifyBuild

import Auth.Domain (CompanyID)
import qualified BackgroundJobs.Runner as Runner

data Job =
  NotifyBuild NotifyBuild.Params

type NotifyBuildConstraint m = NotifyBuild.CallConstraint m

type JobMonad m = NotifyBuild.CallConstraint m

encode' :: FromJSON a => Value -> Maybe a
encode' value =
  case fromJSON value of
    Error _ -> Nothing
    Success a -> Just a

deserialize :: Text -> Value -> Maybe Job
deserialize jobName value =
  case jobName of
    "NotifyBuild" -> NotifyBuild <$> encode' value
    _ -> Nothing

serialize :: Job -> (Text, Value)
serialize job =
  case job of
    NotifyBuild params -> ("NotifyBuild", toJSON params)

run :: JobMonad m => Int -> Job -> m Runner.JobReturnType
run _ job =
  case job of
    NotifyBuild params -> NotifyBuild.call params

config :: JobMonad m => Runner.JobConfig m Job
config = Runner.JobConfig deserialize serialize run

queue :: JobMonad m => Job -> m ()
queue = Runner.queue config

runNext :: JobMonad m => m ()
runNext = Runner.runNext config

notifyBuild :: JobMonad m => CompanyID -> Text -> m ()
notifyBuild cId bId = queue . NotifyBuild $ NotifyBuild.Params cId bId
