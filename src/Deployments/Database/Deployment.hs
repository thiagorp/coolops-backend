module Deployments.Database.Deployment
  ( createQueuedDeployment
  , getNextQueuedDeployment
  , saveRunningDeployment
  , listAllRunningDeployments
  ) where

import RIO

import Data.Time
import Database.PostgreSQL.Simple

import Common.Database
import Deployments.Domain.Deployment
import Deployments.Domain.Project (CompanyID)

queuedStatus :: Text
queuedStatus = "waiting"

runningStatus :: Text
runningStatus = "running"

getNextQueuedDeployment ::
     (HasPostgres m) => CompanyID -> m (Maybe QueuedDeployment)
getNextQueuedDeployment companyId = do
  result <- runQuery q (companyId, queuedStatus)
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

listAllRunningDeployments :: (HasPostgres m) => m [RunningDeployment]
listAllRunningDeployments = do
  results <- runQuery q (Only runningStatus)
  return $ map buildRunningDeployment results
  where
    q =
      "select id, deployment_started_at from deployments\
        \ where status = ?"

saveRunningDeployment :: (HasPostgres m) => RunningDeployment -> m ()
saveRunningDeployment RunningDeployment {..} = runDb' q values
  where
    q =
      "update deployments set (deployment_started_at, status, updated_at) =\
        \ (?, ?, NOW()) where id = ?"
    values = (deploymentStartedAt, runningStatus, runningDeploymentId)

createQueuedDeployment :: (HasPostgres m) => QueuedDeployment -> m ()
createQueuedDeployment QueuedDeployment {..} = runDb' q values
  where
    q =
      "insert into deployments (id, build_id, environment_id, status, created_at, updated_at) values\
        \ (?, ?, ?, ?, NOW(), NOW())"
    values =
      (deploymentId, deploymentBuildId, deploymentEnvironmentId, queuedStatus)

type QueuedDeploymentRow = (ID, BuildID, EnvironmentID)

buildQueuedDeployment :: QueuedDeploymentRow -> QueuedDeployment
buildQueuedDeployment (deploymentId, deploymentBuildId, deploymentEnvironmentId) =
  QueuedDeployment {..}

type RunningDeploymentRow = (ID, LocalTime)

buildRunningDeployment :: RunningDeploymentRow -> RunningDeployment
buildRunningDeployment (runningDeploymentId, deploymentStartedAtLocaltime) =
  let deploymentStartedAt = localTimeToUTC utc deploymentStartedAtLocaltime
   in RunningDeployment {..}
