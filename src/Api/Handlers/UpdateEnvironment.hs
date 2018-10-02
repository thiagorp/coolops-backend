module Api.Handlers.UpdateEnvironment
  ( patchUpdateEnvironmentR
  ) where

import Api.Import

import Deployments.Database.Environment (getEnvironment)
import Deployments.Domain.Environment (Environment, buildName, buildSlug)
import qualified Deployments.UseCases.UpdateEnvironment as App

data Fields
  = Name
  | Slug
  | EnvVars

instance HasFieldName Fields where
  fieldName field =
    case field of
      Name -> "name"
      Slug -> "slug"
      EnvVars -> "env_vars"

data Request = Request
  { reqEnvironmentName :: !(Maybe Text)
  , reqEnvironmentSlug :: !(Maybe Text)
  , reqEnvironmentEnvVars :: !(Maybe (HashMap Text Text))
  }

instance FromJSON Request where
  parseJSON =
    withObject "request params" $ \o -> do
      reqEnvironmentName <- o .:? fieldName Name
      reqEnvironmentSlug <- o .:? fieldName Slug
      reqEnvironmentEnvVars <- o .:? fieldName EnvVars
      return Request {..}

builder :: Request -> WebValidation App.Params
builder Request {..} = App.Params <$> environmentName <*> environmentSlug <*> environmentEnvVars
  where
    environmentName = required_ Name reqEnvironmentName |>> buildName
    environmentSlug = required_ Slug reqEnvironmentSlug |>> buildSlug
    environmentEnvVars = required_ EnvVars reqEnvironmentEnvVars |>> valid

update :: Environment -> Handler EnvironmentResource
update environment = do
  requestData <- requireJsonBody >>= parseValidatedRequest builder
  updatedEnvironment <- App.call environment requestData
  return $ environmentResource updatedEnvironment

call :: Text -> AuthenticatedUser -> Handler Value
call environmentId (AuthenticatedUser user) = do
  maybeEnvironment <- getEnvironment (userCompanyId user) environmentId
  case maybeEnvironment of
    Just environment -> toJSON <$> update environment
    Nothing -> notFound

patchUpdateEnvironmentR :: Text -> Handler Value
patchUpdateEnvironmentR = userAuth . call
