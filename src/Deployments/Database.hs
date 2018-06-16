module Deployments.Database
  ( createProject
  , getProject
  , listProjects
  , updateProject
  ) where

import RIO

import Database.PostgreSQL.Simple

import Common.Database
import Deployments.Domain

createProject :: (HasPostgres m) => Project -> m ()
createProject Project {..} = runDb' q values
  where
    q =
      "insert into projects (id, name, deployment_image, company_id, created_at, updated_at) values\
        \ (?, ?, ?, ?, NOW(), NOW())"
    values = (projectId, projectName, projectDeploymentImage, projectCompanyId)

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
      "select id, name, deployment_image, company_id from projects\
        \ where company_id = ?"

getProject :: (HasPostgres m) => CompanyID -> Text -> m (Maybe Project)
getProject companyId projectId = do
  result <- runQuery q (companyId, projectId)
  case result of
    [] -> return Nothing
    row:_ -> return . Just $ buildProject row
  where
    q =
      "select id, name, deployment_image, company_id from projects\
        \ where company_id = ? AND id = ?"

type ProjectRow = (ProjectID, ProjectName, ProjectDeploymentImage, CompanyID)

buildProject :: ProjectRow -> Project
buildProject (projectId, projectName, projectDeploymentImage, projectCompanyId) =
  Project {..}
