module Deployments.Database.Deployment
  ( createQueuedDeployment
  , getNextQueuedDeployment
  , saveRunningDeployment
  , saveFinishedDeployment
  , listAllRunningDeployments
  , getDeploymentResources
  , notFinishedStatuses
  , DbStatus(..)
  , isFinished
  ) where

import RIO

import Data.Time
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField

import Common.Database
import Deployments.Database.Build
import Deployments.Database.Environment
import Deployments.Database.Project
import Deployments.Domain.Deployment

import Deployments.Domain.Build (buildProjectId)
import Deployments.Domain.Project (CompanyID)
import Util.Key

data DbStatus
  = DbQueued
  | DbRunning
  | DbSucceeded
  | DbFailed Text
  deriving (Eq)

instance FromField DbStatus where
  fromField f bs = textToStatus <$> fromField f bs

instance ToField DbStatus where
  toField = toField . statusText

isFinished :: DbStatus -> Bool
isFinished s = s `notElem` notFinishedStatuses

notFinishedStatuses :: [DbStatus]
notFinishedStatuses = [DbQueued, DbRunning]

textToStatus :: Text -> DbStatus
textToStatus text =
  case text of
    "queued" -> DbQueued
    "running" -> DbRunning
    "finished_with_success" -> DbSucceeded
    "failed_with_invalid_docker_image" -> DbFailed "invalid_docker_image"
    "failed_with_job_failed" -> DbFailed "job_failed"
    "failed_with_job_not_found" -> DbFailed "job_not_found"
    _ -> DbFailed "unknown_reason"

statusText :: DbStatus -> Text
statusText status =
  case status of
    DbQueued -> "queued"
    DbRunning -> "running"
    DbSucceeded -> "finished_with_success"
    DbFailed reason -> "failed_with_" <> reason

convertFinishedtDeploymentStatus :: Status -> DbStatus
convertFinishedtDeploymentStatus status =
  case status of
    Succeeded -> DbSucceeded
    Failed InvalidDockerImage -> DbFailed "invalid_docker_image"
    Failed JobFailed -> DbFailed "job_failed"
    Failed JobNotFound -> DbFailed "job_not_found"

getNextQueuedDeployment ::
     (HasPostgres m) => CompanyID -> m (Maybe QueuedDeployment)
getNextQueuedDeployment companyId = do
  result <- runQuery q (companyId, statusText DbQueued)
  case result of
    [] -> return Nothing
    row:_ -> return . Just $ buildQueuedDeployment row
  where
    q =
      "select d.id, d.build_id, d.environment_id from deployments d\
        \ left join environments e on e.id = d.environment_id\
        \ left join projects p on p.id = e.project_id\
        \ where p.company_id = ? and d.status = ? \
        \ order by d.created_at asc limit 1\
        \ for update skip locked"

getDeploymentResources ::
     (HasPostgres m)
  => CompanyID
  -> Text
  -> Text
  -> m (Maybe DeploymentResources)
getDeploymentResources cId eId bId = do
  maybeEnvironment <- getEnvironment cId eId
  maybeBuild <- getBuild cId bId
  case (,) <$> maybeEnvironment <*> maybeBuild of
    Nothing -> return Nothing
    Just (environment, build) -> do
      maybeProject <- getProject cId $ keyText $ buildProjectId build
      case maybeProject of
        Nothing -> return Nothing
        Just project ->
          return $ Just $ DeploymentResources project environment build

listAllRunningDeployments ::
     (HasPostgres m) => m [(CompanyID, RunningDeployment)]
listAllRunningDeployments = do
  results <- runQuery q (Only (statusText DbRunning))
  return $ map buildRunningDeployment results
  where
    q =
      "select d.id, d.deployment_started_at, d.build_id, p.company_id from deployments d\
        \ join environments e on e.id = d.environment_id\
        \ join projects p on p.id = e.project_id\
        \ where status = ?"

saveFinishedDeployment :: (HasPostgres m) => FinishedDeployment -> m ()
saveFinishedDeployment FinishedDeployment {..} = runDb' q values
  where
    q =
      "update deployments set (deployment_finished_at, status, updated_at) =\
        \ (?, ?, NOW()) where id = ?"
    values =
      ( deploymentFinishedAt
      , statusText $ convertFinishedtDeploymentStatus deploymentStatus
      , finishedDeploymentId)

saveRunningDeployment :: (HasPostgres m) => RunningDeployment -> m ()
saveRunningDeployment RunningDeployment {..} = runDb' q values
  where
    q =
      "update deployments set (deployment_started_at, status, updated_at) =\
        \ (?, ?, NOW()) where id = ?"
    values = (deploymentStartedAt, statusText DbRunning, runningDeploymentId)

createQueuedDeployment :: (HasPostgres m) => QueuedDeployment -> m ()
createQueuedDeployment QueuedDeployment {..} = runDb' q values
  where
    q =
      "insert into deployments (id, build_id, environment_id, status, created_at, updated_at) values\
        \ (?, ?, ?, ?, NOW(), NOW())"
    values =
      ( deploymentId
      , deploymentBuildId
      , deploymentEnvironmentId
      , statusText DbQueued)

type QueuedDeploymentRow = (ID, BuildID, EnvironmentID)

buildQueuedDeployment :: QueuedDeploymentRow -> QueuedDeployment
buildQueuedDeployment (deploymentId, deploymentBuildId, deploymentEnvironmentId) =
  QueuedDeployment {..}

type RunningDeploymentRow = (ID, LocalTime, BuildID, CompanyID)

buildRunningDeployment :: RunningDeploymentRow -> (CompanyID, RunningDeployment)
buildRunningDeployment (runningDeploymentId, deploymentStartedAtLocaltime, runningDeploymentBuildId, companyId) =
  let deploymentStartedAt = localTimeToUTC utc deploymentStartedAtLocaltime
   in (companyId, RunningDeployment {..})
