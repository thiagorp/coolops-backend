module GraphQL.Database.Queries
  ( module GraphQL.Database.Types
  , CompanyID
  , listBuilds
  , listBuildsById
  , listEnvironments
  , listEnvsLastDeployments
  , listProjects
  , listProjectsById
  , listSlackProjectIntegrations
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
      \limit ? offset ? \
      \order by b.created_at desc"

listBuildsById :: (HasPostgres m) => CompanyID -> [BuildID] -> m [Build]
listBuildsById companyId ids = runQuery q (companyId, In ids)
  where
    q =
      "select b.id, b.name, b.params, b.metadata, b.project_id, cast(extract(epoch from b.created_at) as integer), cast(extract(epoch from b.updated_at) as integer) from builds b \
      \join projects p on p.id = b.project_id \
      \where p.company_id = ? and b.id in ?"

listProjects :: (HasPostgres m) => CompanyID -> m [Project]
listProjects companyId = runQuery q (Only companyId)
  where
    q =
      "select id, name, deployment_image, access_token, cast(extract(epoch from created_at) as integer), cast(extract(epoch from updated_at) as integer) from projects \
      \where company_id = ? \
      \order by name asc"

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
      \join projects p on p.id = e.project_id \
      \where p.company_id = ? and p.id in ? \
      \order by e.name asc"

listEnvsLastDeployments ::
     (HasPostgres m) => CompanyID -> [EnvironmentID] -> m [Deployment]
listEnvsLastDeployments companyId envIds = runQuery q (companyId, In envIds)
  where
    q =
      "select d.id, cast(extract(epoch from d.deployment_started_at) as integer), d.environment_id, d.build_id, d.status, cast(extract(epoch from e.created_at) as integer), cast(extract(epoch from e.updated_at) as integer) from deployments d \
      \join environments e on d.environment_id = e.id \
      \join projects p on e.project_id = p.id \
      \where p.company_id = ? and e.id in ? \
      \order by p.id, e.id, d.created_at desc"

listSlackProjectIntegrations ::
     (HasPostgres m) => CompanyID -> [ProjectID] -> m [SlackProjectIntegration]
listSlackProjectIntegrations companyId projectIds =
  runQuery q (companyId, In projectIds)
  where
    q =
      "select spi.project_id, spi.workspace_name from slack_project_integrations spi \
      \join projects p on p.id = spi.project_id \
      \where p.company_id = ? and p.id in ?"