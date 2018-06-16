module Deployments.UseCases.UpdateProject where

import RIO

import Deployments.Classes (ProjectRepo, updateProject)
import Deployments.Domain (Project(..), ProjectDeploymentImage, ProjectName)

data Params = Params
  { paramProjectName :: !ProjectName
  , paramProjectDeploymentImage :: !ProjectDeploymentImage
  }

apply :: Project -> Params -> Project
apply project (Params {..}) = do
  project
    { projectName = paramProjectName
    , projectDeploymentImage = paramProjectDeploymentImage
    }

call :: (ProjectRepo m) => Project -> Params -> m Project
call project params = do
  let updatedProject = apply project params
  updateProject updatedProject
  return updatedProject
