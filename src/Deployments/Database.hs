module Deployments.Database
  ( createBuild
  , createProject
  , createEnvironment
  , findProjectByAccessToken
  , getProject
  , listProjects
  , updateProject
  ) where

import RIO

import Data.Aeson (toJSON)
import Database.PostgreSQL.Simple

import Common.Database
import Deployments.Domain

createBuild :: (HasPostgres m) => Build -> m ()
createBuild Build {..} = runDb' q values
  where
    q =
      "insert into builds (id, name, params, project_id, created_at, updated_at) values\
        \ (?, ?, ?, ?, NOW(), NOW())"
    values = (buildId, buildName, toJSON buildParams, buildProjectId)

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
listProjects companyId =
  runQuery q (Only companyId) >>= return . (map buildProject)
  where
    q =
      "select id, name, deployment_image, company_id, access_token from projects\
        \ where company_id = ?"

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

type ProjectRow
   = ( ProjectID
     , ProjectName
     , ProjectDeploymentImage
     , CompanyID
     , ProjectAccessToken)

buildProject :: ProjectRow -> Project
buildProject (projectId, projectName, projectDeploymentImage, projectCompanyId, projectAccessToken) =
  Project {..}

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
