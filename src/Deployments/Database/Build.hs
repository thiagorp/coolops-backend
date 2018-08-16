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
    row:_ -> return . Just $ build row
  where
    q =
      "select b.id, b.name, b.params, b.metadata, b.project_id from builds b\
        \ left join projects p on p.id = b.project_id\
        \ where p.company_id = ? and b.id = ?"

createBuild :: (HasPostgres m) => Build -> m ()
createBuild Build {..} = runDb' q values
  where
    q =
      "insert into builds (id, name, params, metadata, project_id, created_at, updated_at) values\
        \ (?, ?, ?, ?, ?, NOW(), NOW())"
    values =
      ( buildId
      , buildName
      , toJSON buildParams
      , toJSON buildMetadata
      , buildProjectId)

type BuildRow = (ID, Name, Value, Value, ProjectID)

build :: BuildRow -> Build
build (buildId, buildName, params, metadata, buildProjectId) =
  let buildParams =
        case fromJSON params of
          Error _ -> HashMap.empty
          Success p -> p
      buildMetadata =
        case fromJSON metadata of
          Error _ -> HashMap.empty
          Success m -> m
   in Build {..}
