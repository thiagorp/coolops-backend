module Handlers.UpdateEnvironment
  ( call
  ) where

import RIO

import Data.Aeson hiding (json)
import Network.HTTP.Types.Status (notFound404)
import Web.Scotty.Trans

import Authorization (AuthenticatedUser(..), User(..))
import Deployments.Classes (getEnvironment)
import Deployments.Domain.Environment (Environment, buildName, buildSlug)
import qualified Deployments.UseCases.UpdateEnvironment as App
import Resources
import Types
import Validation

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
    environmentName = required Name reqEnvironmentName |>> buildName
    environmentSlug = required Slug reqEnvironmentSlug |>> buildSlug
    environmentEnvVars = required EnvVars reqEnvironmentEnvVars |>> valid

update :: Environment -> WebMonad ()
update environment = do
  requestData <- jsonData >>= parseRequest builder
  updatedEnvironment <- lift $ App.call environment requestData
  json $ environmentResource updatedEnvironment

call :: AuthenticatedUser -> WebMonad ()
call (AuthenticatedUser user) = do
  environmentId <- param "id"
  maybeEnvironment <- lift $ getEnvironment (userCompanyId user) environmentId
  case maybeEnvironment of
    Just environment -> update environment
    Nothing -> status notFound404
