module Resources where

import RIO

import Data.Aeson hiding (json)

import Deployments.Domain
import Util.Key (keyText)

data ProjectResource = ProjectResource
  { resProjectId :: !Text
  , resProjectName :: !Text
  , resProjectDeploymentImage :: !Text
  , resProjectAccessToken :: !Text
  }

instance ToJSON ProjectResource where
  toJSON ProjectResource {..} =
    object
      [ "id" .= resProjectId
      , "name" .= resProjectName
      , "deployment_image" .= resProjectDeploymentImage
      , "access_token" .= resProjectAccessToken
      ]

projectResource :: Project -> ProjectResource
projectResource Project {..} =
  let resProjectId = keyText projectId
      resProjectName = projectNameText projectName
      resProjectAccessToken = projectAccessTokenText projectAccessToken
      resProjectDeploymentImage =
        projectDeploymentImageText projectDeploymentImage
   in ProjectResource {..}

data EnvironmentResource = EnvironmentResource
  { resEnvironmentId :: !Text
  , resEnvironmentName :: !Text
  , resEnvironmentProjectId :: !Text
  , resEnvironmentEnvVars :: !(HashMap Text Text)
  }

instance ToJSON EnvironmentResource where
  toJSON EnvironmentResource {..} =
    object
      [ "id" .= resEnvironmentId
      , "name" .= resEnvironmentName
      , "project_id" .= resEnvironmentProjectId
      , "env_vars" .= resEnvironmentEnvVars
      ]

environmentResource :: Environment -> EnvironmentResource
environmentResource Environment {..} =
  let resEnvironmentId = keyText environmentId
      resEnvironmentName = environmentNameText environmentName
      resEnvironmentProjectId = keyText environmentProjectId
      resEnvironmentEnvVars = environmentEnvVars
   in EnvironmentResource {..}
