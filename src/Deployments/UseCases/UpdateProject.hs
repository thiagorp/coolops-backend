module Deployments.UseCases.UpdateProject
  ( Params(..)
  , Project.Project
  , call
  ) where

import RIO

import Common.Database
import Deployments.Database.Project (updateProject)
import qualified Deployments.Domain.Project as Project

data Params = Params
  { paramProjectName :: !Project.Name
  , paramProjectSlug :: !Project.Slug
  , paramProjectDeploymentImage :: !Project.DeploymentImage
  }

apply :: Project.Project -> Params -> Project.Project
apply project Params {..} =
  project
    { Project.projectName = paramProjectName
    , Project.projectSlug = paramProjectSlug
    , Project.projectDeploymentImage = paramProjectDeploymentImage
    }

call :: (HasPostgres m) => Project.Project -> Params -> m Project.Project
call project params = do
  let updatedProject = apply project params
  updateProject updatedProject
  return updatedProject
