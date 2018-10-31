module Api.Resources where

import RIO

import Data.Aeson hiding (json)

import Model

data ProjectResource = ProjectResource
  { resProjectId :: !ProjectId
  , resProjectName :: !ProjectName
  , resProjectDeploymentImage :: !DockerImage
  , resProjectAccessToken :: !AccessToken
  }

instance ToJSON ProjectResource where
  toJSON ProjectResource {..} =
    object
      [ "id" .= resProjectId
      , "name" .= resProjectName
      , "deployment_image" .= resProjectDeploymentImage
      , "access_token" .= resProjectAccessToken
      ]

projectResource :: Entity Project -> ProjectResource
projectResource (Entity projectId Project {..}) =
  let resProjectId = projectId
      resProjectName = projectName
      resProjectAccessToken = projectAccessToken
      resProjectDeploymentImage = projectDeploymentImage
   in ProjectResource {..}

data EnvironmentResource = EnvironmentResource
  { resEnvironmentId :: !EnvironmentId
  , resEnvironmentName :: !EnvironmentName
  , resEnvironmentProjectId :: !ProjectId
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

environmentResource :: Entity Environment -> EnvironmentResource
environmentResource (Entity environmentId Environment {..}) =
  let resEnvironmentId = environmentId
      resEnvironmentName = environmentName
      resEnvironmentProjectId = environmentProjectId
      resEnvironmentEnvVars = environmentEnvVars
   in EnvironmentResource {..}
