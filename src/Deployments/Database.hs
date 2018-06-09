module Deployments.Database
  ( createProject
  , listProjects
  ) where

import RIO

import Database.PostgreSQL.Simple

import Common.Database
import Deployments.Domain

createProject :: (HasPostgres m) => Project -> m ()
createProject Project {..} = runDb' q values
  where
    q =
      "insert into projects (id, name, company_id, created_at, updated_at) values\
        \ (?, ?, ?, NOW(), NOW())"
    values = (projectId, projectName, projectCompanyId)

listProjects :: (HasPostgres m) => CompanyID -> m [Project]
listProjects companyId =
  runQuery q (Only companyId) >>= return . (map buildProject)
  where
    q =
      "select id, name, company_id from projects\
        \ where company_id = ?"

type ProjectRow = (ProjectID, ProjectName, CompanyID)

buildProject :: ProjectRow -> Project
buildProject (projectId, projectName, projectCompanyId) = Project {..}
