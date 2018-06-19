module Deployments.Database.Build
  ( createBuild
  , getBuild
  ) where

import RIO
import qualified RIO.HashMap as HashMap

import Data.Aeson (Result(..), Value, fromJSON, toJSON)

import Common.Database
import Deployments.Domain.Build
import Deployments.Domain.Project (CompanyID)

getBuild :: (HasPostgres m) => CompanyID -> Text -> m (Maybe Build)
getBuild companyId buildId = do
  result <- runQuery q (companyId, buildId)
  case result of
    [] -> return Nothing
    row:_ -> return . Just $ buildBuild row
  where
    q =
      "select b.id, b.name, b.params, b.project_id from builds b\
        \ left join projects p on p.id = b.project_id\
        \ where p.company_id = ? and b.id = ?"

createBuild :: (HasPostgres m) => Build -> m ()
createBuild Build {..} = runDb' q values
  where
    q =
      "insert into builds (id, name, params, project_id, created_at, updated_at) values\
        \ (?, ?, ?, ?, NOW(), NOW())"
    values = (buildId, buildName, toJSON buildParams, buildProjectId)

type BuildRow = (ID, Name, Value, ProjectID)

buildBuild :: BuildRow -> Build
buildBuild (buildId, buildName, params, buildProjectId) =
  let buildParams =
        case fromJSON params of
          Error _ -> HashMap.empty
          Success p -> p
   in Build {..}
