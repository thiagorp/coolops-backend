module Deployments.Database where

import Common.Database
import Deployments.Domain

createProject :: (HasPostgres m) => Project -> m ()
createProject Project {..} = runDb' q values
  where
    q =
      "insert into projects (id, name, company_id, created_at, updated_at) values\
        \ (?, ?, ?, NOW(), NOW())"
    values = (projectId, projectName, projectCompanyId)
