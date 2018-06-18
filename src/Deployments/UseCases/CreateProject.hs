module Deployments.UseCases.CreateProject where

import RIO

import Deployments.Classes (ProjectRepo, createProject)
import qualified Deployments.Domain.Project as Project

data Params = Params
  { projectName :: !Project.Name
  , projectDeploymentImage :: !Project.DeploymentImage
  , companyId :: !Project.CompanyID
  }

build :: (MonadIO m) => Params -> m Project.Project
build Params {..} = do
  projectId <- Project.genId
  projectAccessToken <- Project.genAccessToken
  let projectCompanyId = companyId
  return Project.Project {..}

call :: (MonadIO m, ProjectRepo m) => Params -> m Project.Project
call params = do
  project <- build params
  createProject project
  return project
