module Resources where

import RIO

import Data.Aeson hiding (json)

import Deployments.Domain
import Util.Key (keyText)

data ProjectResource = ProjectResource
  { resProjectId :: !Text
  , resProjectName :: !Text
  , resProjectDeploymentImage :: !Text
  }

instance ToJSON ProjectResource where
  toJSON ProjectResource {..} =
    object
      [ "id" .= resProjectId
      , "name" .= resProjectName
      , "deployment_image" .= resProjectDeploymentImage
      ]

projectResource :: Project -> ProjectResource
projectResource Project {..} =
  let resProjectId = keyText projectId
      resProjectName = projectNameText projectName
      resProjectDeploymentImage =
        projectDeploymentImageText projectDeploymentImage
   in ProjectResource {..}
