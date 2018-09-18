module Handlers.CreateEnvironment
  ( call
  ) where

import RIO

import Data.Aeson hiding (json)
import Network.HTTP.Types.Status (conflict409, created201, notFound404)
import Web.Scotty.Trans

import Authorization (AuthenticatedUser(..), User(..))
import Deployments.Domain.Environment (buildName, buildSlug)
import qualified Deployments.UseCases.CreateEnvironment as App
import Resources
import Types
import Validation

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
    environmentName = required Name reqEnvironmentName |>> buildName
    environmentEnvVars = required EnvVars reqEnvironmentEnvVars |>> valid
    environmentSlug = required Slug reqEnvironmentSlug |>> buildSlug

call :: AuthenticatedUser -> WebMonad ()
call (AuthenticatedUser User {..}) = do
  projectId <- param "project_id"
  requestData <- jsonData >>= parseRequest builder
  maybeProject <- lift $ App.call projectId userCompanyId requestData
  case maybeProject of
    Left App.ProjectNotFound -> status notFound404
    Left App.SlugAlreadyExists -> status conflict409
    Right project -> status created201 >> json (environmentResource project)
