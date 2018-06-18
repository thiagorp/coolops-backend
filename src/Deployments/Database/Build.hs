module Deployments.Database.Build
  ( createBuild
  ) where

import Data.Aeson (toJSON)

import Common.Database
import Deployments.Domain.Build

createBuild :: (HasPostgres m) => Build -> m ()
createBuild Build {..} = runDb' q values
  where
    q =
      "insert into builds (id, name, params, project_id, created_at, updated_at) values\
        \ (?, ?, ?, ?, NOW(), NOW())"
    values = (buildId, buildName, toJSON buildParams, buildProjectId)
