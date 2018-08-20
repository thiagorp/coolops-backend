module Deployments.Database.Environment
  ( createEnvironment
  , getEnvironment
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
      "select e.id, e.name, e.env_vars, e.project_id from environments e\
        \ left join projects p on p.id = e.project_id\
        \ where p.company_id = ? and e.id = ?"

listEnvironments :: (HasPostgres m) => CompanyID -> m [Environment]
listEnvironments companyId =
  map buildEnvironment <$> runQuery q (Only companyId)
  where
    q =
      "select e.id, e.name, e.env_vars, e.project_id from environments e\
        \ left join projects p on p.id = e.project_id\
        \ where p.company_id = ?"

listProjectEnvironments ::
     (HasPostgres m) => CompanyID -> Text -> m [Environment]
listProjectEnvironments companyId projectId =
  map buildEnvironment <$> runQuery q (companyId, projectId)
  where
    q =
      "select e.id, e.name, e.env_vars, e.project_id from environments e\
        \ left join projects p on p.id = e.project_id\
        \ where p.company_id = ? and p.id = ?"

listEnvironmentsForProjects ::
     (HasPostgres m) => CompanyID -> [ProjectID] -> m [Environment]
listEnvironmentsForProjects companyId projectIds =
  map buildEnvironment <$> runQuery q (companyId, In projectIds)
  where
    q =
      "select e.id, e.name, e.env_vars, e.project_id from environments e\
        \ left join projects p on p.id = e.project_id\
        \ where p.company_id = ? and p.id in ?"

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

updateEnvironment :: (HasPostgres m) => Environment -> m ()
updateEnvironment Environment {..} = runDb' q values
  where
    q =
      "update environments set (name, env_vars, updated_at) =\
        \ (?, ?, NOW()) where id = ?"
    values = (environmentName, toJSON environmentEnvVars, environmentId)

type EnvironmentRow = (ID, Name, Value, ProjectID)

buildEnvironment :: EnvironmentRow -> Environment
buildEnvironment (environmentId, environmentName, envVars, environmentProjectId) =
  let environmentEnvVars =
        case fromJSON envVars of
          Error _ -> HashMap.empty
          Success p -> p
   in Environment {..}
