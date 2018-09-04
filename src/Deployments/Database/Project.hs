module Deployments.Database.Project
  ( DBProjectID
  , createProject
  , findProjectByAccessToken
  , getProject
  , getProjectForBuild
  , listProjects
  , updateProject
  ) where

import RIO

import Database.PostgreSQL.Simple

import Common.Database
import qualified Deployments.Domain.Build as B
import Deployments.Domain.Project
import Util.Key (keyText)

class DBProjectID a where
  toDbId :: a -> Text

instance DBProjectID Text where
  toDbId = id

instance DBProjectID ID where
  toDbId = keyText

createProject :: (HasPostgres m) => Project -> m ()
createProject Project {..} = runDb' q values
  where
    q =
      "insert into projects (id, name, deployment_image, company_id, access_token, created_at, updated_at) values\
        \ (?, ?, ?, ?, ?, now() at time zone 'utc', now() at time zone 'utc')"
    values = (projectId, projectName, projectDeploymentImage, projectCompanyId, projectAccessToken)

updateProject :: (HasPostgres m) => Project -> m ()
updateProject Project {..} = runDb' q (projectName, projectDeploymentImage, projectId)
  where
    q =
      "update projects set (name, deployment_image, updated_at) =\
        \ (?, ?, now() at time zone 'utc') where id = ?"

listProjects :: (HasPostgres m) => CompanyID -> m [Project]
listProjects companyId = map buildProject <$> runQuery q (Only companyId)
  where
    q =
      "select id, name, deployment_image, company_id, access_token from projects\
        \ where company_id = ?"

getProject :: (HasPostgres m, DBProjectID a) => CompanyID -> a -> m (Maybe Project)
getProject companyId projectId = do
  result <- runQuery q (companyId, toDbId projectId)
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
buildProject (projectId, projectName, projectDeploymentImage, projectCompanyId, projectAccessToken) = Project {..}
