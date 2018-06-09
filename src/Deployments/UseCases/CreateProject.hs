module Deployments.UseCases.CreateProject where

import RIO

import Deployments.Classes (ProjectRepo, createProject)
import Deployments.Domain (CompanyID, Project(..), ProjectName, genProjectId)

data Params = Params
  { projectName :: !ProjectName
  , companyId :: !CompanyID
  }

build :: (MonadIO m) => Params -> m Project
build Params {..} = do
  projectId <- genProjectId
  let projectCompanyId = companyId
  return Project {..}

call :: (MonadIO m, ProjectRepo m) => Params -> m Project
call params = do
  project <- build params
  createProject project
  return project
