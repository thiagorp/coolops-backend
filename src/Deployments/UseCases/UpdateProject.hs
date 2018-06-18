module Deployments.UseCases.UpdateProject where

import RIO

import Deployments.Classes (ProjectRepo, updateProject)
import qualified Deployments.Domain.Project as Project

data Params = Params
  { paramProjectName :: !Project.Name
  , paramProjectDeploymentImage :: !Project.DeploymentImage
  }

apply :: Project.Project -> Params -> Project.Project
apply project (Params {..}) = do
  project
    { Project.projectName = paramProjectName
    , Project.projectDeploymentImage = paramProjectDeploymentImage
    }

call :: (ProjectRepo m) => Project.Project -> Params -> m Project.Project
call project params = do
  let updatedProject = apply project params
  updateProject updatedProject
  return updatedProject
