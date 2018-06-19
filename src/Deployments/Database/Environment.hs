module Deployments.Database.Environment
  ( createEnvironment
  , getEnvironment
  ) where

import RIO
import qualified RIO.HashMap as HashMap

import Data.Aeson (Result(..), Value, fromJSON, toJSON)

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

type EnvironmentRow = (ID, Name, Value, ProjectID)

buildEnvironment :: EnvironmentRow -> Environment
buildEnvironment (environmentId, environmentName, envVars, environmentProjectId) =
  let environmentEnvVars =
        case fromJSON envVars of
          Error _ -> HashMap.empty
          Success p -> p
   in Environment {..}
