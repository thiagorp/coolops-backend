module Deployments.Database.Environment
  ( createEnvironment
  ) where

import Data.Aeson (toJSON)

import Common.Database
import Deployments.Domain.Environment

createEnvironment :: (HasPostgres m) => Environment -> m ()
createEnvironment Environment {..} = runDb' q values
  where
    q =
      "insert into environments (id, name, project_id, env_vars, created_at, updated_at) values\
        \ (?, ?, ?, ?, NOW(), NOW())"
    values =
      ( environmentId
      , environmentName
      , environmentProjectId
      , toJSON environmentEnvVars)
