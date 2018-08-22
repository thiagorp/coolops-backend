module GraphQL.Database.Queries
  ( module GraphQL.Database.Types
  , CompanyID
  , listBuilds
  , listEnvironments
  , listProjects
  , listProjectsById
  ) where

import RIO

import Database.PostgreSQL.Simple

import Auth.Domain (CompanyID)
import Common.Database
import GraphQL.Database.Types

listBuilds :: (HasPostgres m) => (Int, Int) -> CompanyID -> m [Build]
listBuilds (limit, offset) companyId = runQuery q (companyId, limit, offset)
  where
    q =
      "select b.id, b.name, b.params, b.metadata, b.project_id, cast(extract(epoch from b.created_at) as integer), cast(extract(epoch from b.updated_at) as integer) from builds b \
      \join projects p on p.id = b.project_id \
      \where p.company_id = ? \
      \limit ? offset ?"

listProjects :: (HasPostgres m) => CompanyID -> m [Project]
listProjects companyId = runQuery q (Only companyId)
  where
    q =
      "select id, name, deployment_image, access_token, cast(extract(epoch from created_at) as integer), cast(extract(epoch from updated_at) as integer) from projects \
      \where company_id = ?"

listProjectsById :: (HasPostgres m) => CompanyID -> [ProjectID] -> m [Project]
listProjectsById companyId ids = runQuery q (companyId, In ids)
  where
    q =
      "select id, name, deployment_image, access_token, cast(extract(epoch from created_at) as integer), cast(extract(epoch from updated_at) as integer) from projects \
      \where company_id = ? and id in ?"

listEnvironments ::
     (HasPostgres m) => CompanyID -> [ProjectID] -> m [Environment]
listEnvironments companyId projectIds = runQuery q (companyId, In projectIds)
  where
    q =
      "select e.id, e.name, e.env_vars, e.project_id, cast(extract(epoch from e.created_at) as integer), cast(extract(epoch from e.updated_at) as integer) from environments e \
      \left join projects p on p.id = e.project_id \
      \where p.company_id = ? and p.id in ?"
