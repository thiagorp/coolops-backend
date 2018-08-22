module Deployments.Database.Project
  ( createProject
  , findProjectByAccessToken
  , getProject
  , getProjectForBuild
  , listProjects
  , listProjectsById
  , updateProject
  ) where

import RIO

import Database.PostgreSQL.Simple

import Common.Database
import qualified Deployments.Domain.Build as B
import Deployments.Domain.Project
import Util.Key (keyText)

class PostgresProjectId a where
  toId :: a -> Text

instance PostgresProjectId Text where
  toId = id

instance PostgresProjectId ID where
  toId = keyText

createProject :: (HasPostgres m) => Project -> m ()
createProject Project {..} = runDb' q values
  where
    q =
      "insert into projects (id, name, deployment_image, company_id, access_token, created_at, updated_at) values\
        \ (?, ?, ?, ?, ?, NOW(), NOW())"
    values =
      ( projectId
      , projectName
      , projectDeploymentImage
      , projectCompanyId
      , projectAccessToken)

updateProject :: (HasPostgres m) => Project -> m ()
updateProject Project {..} =
  runDb' q (projectName, projectDeploymentImage, projectId)
  where
    q =
      "update projects set (name, deployment_image, updated_at) =\
        \ (?, ?, NOW()) where id = ?"

listProjects :: (HasPostgres m) => CompanyID -> m [Project]
listProjects companyId = map buildProject <$> runQuery q (Only companyId)
  where
    q =
      "select id, name, deployment_image, company_id, access_token from projects\
        \ where company_id = ?"

listProjectsById ::
     (HasPostgres m, PostgresProjectId a) => CompanyID -> [a] -> m [Project]
listProjectsById companyId projectIds =
  map buildProject <$> runQuery q (In ids, companyId)
  where
    ids = map toId projectIds
    q =
      "select id, name, deployment_image, company_id, access_token from projects\
        \ where id in ? and company_id = ?"

getProject :: (HasPostgres m) => CompanyID -> Text -> m (Maybe Project)
getProject companyId projectId = do
  result <- runQuery q (companyId, projectId)
  case result of
    [] -> return Nothing
    row:_ -> return . Just $ buildProject row
  where
    q =
      "select id, name, deployment_image, company_id, access_token from projects\
        \ where company_id = ? AND id = ?"

getProjectForBuild :: (HasPostgres m) => B.Build -> m (Maybe Project)
getProjectForBuild build = do
  result <- runQuery q (Only $ B.buildProjectId build)
  case result of
    [] -> return Nothing
    row:_ -> return . Just $ buildProject row
  where
    q =
      "select id, name, deployment_image, company_id, access_token from projects\
        \ where id = ?"

findProjectByAccessToken :: (HasPostgres m) => Text -> m (Maybe Project)
findProjectByAccessToken accessToken = do
  result <- runQuery q (Only accessToken)
  case result of
    [] -> return Nothing
    row:_ -> return . Just $ buildProject row
  where
    q =
      "select id, name, deployment_image, company_id, access_token from projects\
        \ where access_token = ?"

type ProjectRow = (ID, Name, DeploymentImage, CompanyID, AccessToken)

buildProject :: ProjectRow -> Project
buildProject (projectId, projectName, projectDeploymentImage, projectCompanyId, projectAccessToken) =
  Project {..}
