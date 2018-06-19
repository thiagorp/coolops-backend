module Deployments.Domain.Deployment
  ( Deployment(..)
  , ID
  , BuildID
  , EnvironmentID
  , genId
  , initialStatus
  ) where

import RIO

import Database.PostgreSQL.Simple.FromField
  ( FromField(..)
  , ResultError(..)
  , returnError
  )
import Database.PostgreSQL.Simple.ToField (ToField(..))

import qualified Deployments.Domain.Build as Build
import qualified Deployments.Domain.Environment as Environment
import Util.Key

type ID = Key Deployment

type BuildID = Build.ID

type EnvironmentID = Environment.ID

data CompletionStatus
  = Succeeded
  | Failed

data Status
  = Waiting
  | Queued
  | Running
  | Completed CompletionStatus

instance ToField Status where
  toField status = toField str
    where
      str :: Text
      str =
        case status of
          Waiting -> "waiting"
          Queued -> "queued"
          Running -> "running"
          Completed Succeeded -> "completed_succeeded"
          Completed Failed -> "completed_failed"

instance FromField Status where
  fromField f bs =
    case bs of
      Just "waiting" -> return Waiting
      Just "queued" -> return Queued
      Just "running" -> return Running
      Just "completed_succeeded" -> return (Completed Succeeded)
      Just "completed_failed" -> return (Completed Failed)
      Just _ -> returnError ConversionFailed f "Unrecognized deployment status"
      Nothing ->
        returnError UnexpectedNull f "Deployment status should never be null"

data Deployment = Deployment
  { deploymentId :: !ID
  , deploymentBuildId :: !BuildID
  , deploymentEnvironmentId :: !EnvironmentID
  , deploymentStatus :: !Status
  }

genId :: MonadIO m => m ID
genId = genID

initialStatus :: Status
initialStatus = Waiting
