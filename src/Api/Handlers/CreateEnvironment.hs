module Api.Handlers.CreateEnvironment
  ( postProjectsEnvironmentsR
  ) where

import Api.Import

import Deployments.Domain.Environment (buildName, buildSlug)
import qualified Deployments.UseCases.CreateEnvironment as App

data Fields
  = Name
  | EnvVars
  | Slug

instance HasFieldName Fields where
  fieldName field =
    case field of
      Name -> "name"
      EnvVars -> "env_vars"
      Slug -> "slug"

data Request = Request
  { reqEnvironmentName :: !(Maybe Text)
  , reqEnvironmentEnvVars :: !(Maybe (HashMap Text Text))
  , reqEnvironmentSlug :: !(Maybe Text)
  }

instance FromJSON Request where
  parseJSON =
    withObject "request params" $ \o -> do
      reqEnvironmentName <- o .:? fieldName Name
      reqEnvironmentEnvVars <- o .:? fieldName EnvVars
      reqEnvironmentSlug <- o .:? fieldName Slug
      return Request {..}

builder :: Request -> WebValidation App.Params
builder Request {..} = App.Params <$> environmentName <*> environmentEnvVars <*> environmentSlug
  where
    environmentName = required_ Name reqEnvironmentName |>> buildName
    environmentEnvVars = required_ EnvVars reqEnvironmentEnvVars |>> valid
    environmentSlug = required_ Slug reqEnvironmentSlug |>> buildSlug

call :: Text -> AuthenticatedUser -> Handler Value
call projectId (AuthenticatedUser User {..}) = do
  requestData <- requireJsonBody >>= parseValidatedRequest builder
  maybeProject <- App.call projectId userCompanyId requestData
  case maybeProject of
    Left App.ProjectNotFound -> notFound
    Left App.SlugAlreadyExists -> sendResponseStatus conflict409 ()
    Right project -> sendStatusJSON created201 $ toJSON (environmentResource project)

postProjectsEnvironmentsR :: Text -> Handler Value
postProjectsEnvironmentsR projectId = userAuth (call projectId)
