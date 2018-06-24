module Resources where

import RIO

import Data.Aeson hiding (json)

import qualified Deployments.Domain.Environment as Environment
import qualified Deployments.Domain.Project as Project
import qualified Slack.Domain.Team as SlackTeam
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

data SlackConfigResource = SlackConfigResource
  { resSlackConfigEnabled :: !Bool
  , resSlackConfigClientId :: !Text
  }

instance ToJSON SlackConfigResource where
  toJSON SlackConfigResource {..} =
    object
      [ "enabled" .= resSlackConfigEnabled
      , "client_id" .= resSlackConfigClientId
      ]

slackConfigResource :: Text -> (Maybe SlackTeam.Team) -> SlackConfigResource
slackConfigResource resSlackConfigClientId maybeSlackTeam =
  let resSlackConfigEnabled = isJust maybeSlackTeam
   in SlackConfigResource {..}
