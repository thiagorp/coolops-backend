module Deployments.Database.Environment
  ( createEnvironment
  , getEnvironment
  , getEnvironmentBySlug
  , listEnvironments
  , listProjectEnvironments
  , listEnvironmentsForProjects
  , updateEnvironment
  ) where

import RIO
import qualified RIO.HashMap as HashMap

import Data.Aeson (Result(..), Value, fromJSON, toJSON)
import Database.PostgreSQL.Simple

import Common.Database
import Deployments.Domain.Environment
import Deployments.Domain.Project (CompanyID)

getEnvironment :: (HasPostgres m) => CompanyID -> Text -> m (Maybe Environment)
getEnvironment companyId environmentId = do
  result <- runQuery q (companyId, environmentId)
  case result of
    [] -> return Nothing
    row:_ -> return . Just $ buildEnvironment row
  where
    q =
      "select e.id, e.name, e.env_vars, e.project_id, e.slug from environments e\
        \ left join projects p on p.id = e.project_id\
        \ where p.company_id = ? and e.id = ?"

getEnvironmentBySlug :: (HasPostgres m) => CompanyID -> ProjectID -> Slug -> m (Maybe Environment)
getEnvironmentBySlug companyId pId slug = do
  result <- runQuery q (companyId, pId, slug)
  case result of
    [] -> return Nothing
    row:_ -> return . Just $ buildEnvironment row
  where
    q =
      "select e.id, e.name, e.env_vars, e.project_id, e.slug from environments e\
        \ left join projects p on p.id = e.project_id\
        \ where p.company_id = ? and e.project_id = ? and e.slug = ?"

listEnvironments :: (HasPostgres m) => CompanyID -> m [Environment]
listEnvironments companyId = map buildEnvironment <$> runQuery q (Only companyId)
  where
    q =
      "select e.id, e.name, e.env_vars, e.project_id, e.slug from environments e\
        \ left join projects p on p.id = e.project_id\
        \ where p.company_id = ?"

listProjectEnvironments :: (HasPostgres m) => CompanyID -> Text -> m [Environment]
listProjectEnvironments companyId projectId = map buildEnvironment <$> runQuery q (companyId, projectId)
  where
    q =
      "select e.id, e.name, e.env_vars, e.project_id, e.slug from environments e\
        \ left join projects p on p.id = e.project_id\
        \ where p.company_id = ? and p.id = ?"

listEnvironmentsForProjects :: (HasPostgres m) => CompanyID -> [ProjectID] -> m [Environment]
listEnvironmentsForProjects companyId projectIds = map buildEnvironment <$> runQuery q (companyId, In projectIds)
  where
    q =
      "select e.id, e.name, e.env_vars, e.project_id, e.slug from environments e\
        \ left join projects p on p.id = e.project_id\
        \ where p.company_id = ? and p.id in ?"

createEnvironment :: (HasPostgres m) => Environment -> m ()
createEnvironment Environment {..} = runDb' q values
  where
    q =
      "insert into environments (id, name, project_id, env_vars, slug, created_at, updated_at) values\
        \ (?, ?, ?, ?, ?, now() at time zone 'utc', now() at time zone 'utc')"
    values = (environmentId, environmentName, environmentProjectId, toJSON environmentEnvVars, environmentSlug)

updateEnvironment :: (HasPostgres m) => Environment -> m ()
updateEnvironment Environment {..} = runDb' q values
  where
    q =
      "update environments set (name, env_vars, slug, updated_at) =\
        \ (?, ?, ?, now() at time zone 'utc') where id = ?"
    values = (environmentName, toJSON environmentEnvVars, environmentSlug, environmentId)

type EnvironmentRow = (ID, Name, Value, ProjectID, Slug)

buildEnvironment :: EnvironmentRow -> Environment
buildEnvironment (environmentId, environmentName, envVars, environmentProjectId, environmentSlug) =
  let environmentEnvVars =
        case fromJSON envVars of
          Error _ -> HashMap.empty
          Success p -> p
   in Environment {..}
