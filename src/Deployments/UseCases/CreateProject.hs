module Deployments.UseCases.CreateProject where

import RIO

import Deployments.Classes (ProjectRepo, createProject)
import Deployments.Domain (CompanyID, Project(..), ProjectName, genProjectId)

data CreateProject = CreateProject
  { projectName :: !ProjectName
  , companyId :: !CompanyID
  }

build :: (MonadIO m) => CreateProject -> m Project
build CreateProject {..} = do
  projectId <- genProjectId
  let projectCompanyId = companyId
  return Project {..}

call :: (MonadIO m, ProjectRepo m) => CreateProject -> m Project
call params = do
  project <- build params
  createProject project
  return project
