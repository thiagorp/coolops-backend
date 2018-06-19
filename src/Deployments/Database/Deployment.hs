module Deployments.Database.Deployment
  ( createDeployment
  ) where

import Common.Database
import Deployments.Domain.Deployment

createDeployment :: (HasPostgres m) => Deployment -> m ()
createDeployment Deployment {..} = runDb' q values
  where
    q =
      "insert into deployments (id, build_id, environment_id, status, created_at, updated_at) values\
        \ (?, ?, ?, ?, NOW(), NOW())"
    values =
      ( deploymentId
      , deploymentBuildId
      , deploymentEnvironmentId
      , deploymentStatus)
