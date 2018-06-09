module Deployments.UseCases.UpdateProject where

import RIO

import Deployments.Classes (ProjectRepo, updateProject)
import Deployments.Domain (Project(..), ProjectName)

data UpdateProject = UpdateProject
  { updateProjectName :: !ProjectName
  }

apply :: Project -> UpdateProject -> Project
apply project (UpdateProject {..}) = do
  project {projectName = updateProjectName}

call :: (ProjectRepo m) => Project -> UpdateProject -> m Project
call project params = do
  let updatedProject = apply project params
  updateProject updatedProject
  return updatedProject
