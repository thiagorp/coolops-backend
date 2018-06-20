module Deployments.Database.Deployment
  ( createQueuedDeployment
  ) where

import RIO

import Common.Database
import Deployments.Domain.Deployment

waitingStatus :: Text
waitingStatus = "waiting"

createQueuedDeployment :: (HasPostgres m) => QueuedDeployment -> m ()
createQueuedDeployment QueuedDeployment {..} = runDb' q values
  where
    q =
      "insert into deployments (id, build_id, environment_id, status, created_at, updated_at) values\
        \ (?, ?, ?, ?, NOW(), NOW())"
    values =
      (deploymentId, deploymentBuildId, deploymentEnvironmentId, waitingStatus)
