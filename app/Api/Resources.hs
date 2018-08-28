module Resources where

import RIO

import Data.Aeson hiding (json)

import qualified Deployments.Domain.Environment as Environment
import qualified Deployments.Domain.Project as Project
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

projectResource :: Project.Project -> ProjectResource
projectResource Project.Project {..} =
  let resProjectId = keyText projectId
      resProjectName = Project.nameText projectName
      resProjectAccessToken = Project.accessTokenText projectAccessToken
      resProjectDeploymentImage =
        Project.deploymentImageText projectDeploymentImage
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

environmentResource :: Environment.Environment -> EnvironmentResource
environmentResource Environment.Environment {..} =
  let resEnvironmentId = keyText environmentId
      resEnvironmentName = Environment.nameText environmentName
      resEnvironmentProjectId = keyText environmentProjectId
      resEnvironmentEnvVars = environmentEnvVars
   in EnvironmentResource {..}
