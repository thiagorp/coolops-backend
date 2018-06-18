module Handlers.CreateEnvironment
  ( call
  ) where

import RIO

import Data.Aeson hiding (json)
import Network.HTTP.Types.Status (created201, notFound404)
import Web.Scotty.Trans

import Authorization (AuthenticatedUser(..), User(..))
import Deployments.Domain.Environment (buildName)
import qualified Deployments.UseCases.CreateEnvironment as App
import Resources
import Types
import Validation

data Fields
  = Name
  | EnvVars

instance HasFieldName Fields where
  fieldName field =
    case field of
      Name -> "name"
      EnvVars -> "env_vars"

data Request = Request
  { reqEnvironmentName :: !(Maybe Text)
  , reqEnvironmentEnvVars :: !(Maybe (HashMap Text Text))
  }

instance FromJSON Request where
  parseJSON =
    withObject "request params" $ \o -> do
      reqEnvironmentName <- o .:? (fieldName Name)
      reqEnvironmentEnvVars <- o .:? (fieldName EnvVars)
      return Request {..}

builder :: User -> Text -> Request -> WebValidation App.Params
builder (User {..}) projectId (Request {..}) =
  App.Params <$> environmentName <*> environmentEnvVars <*> (pure projectId) <*>
  (pure userCompanyId)
  where
    environmentName = required Name reqEnvironmentName >>> buildName
    environmentEnvVars = required EnvVars reqEnvironmentEnvVars >>> valid

call :: AuthenticatedUser -> WebMonad ()
call (AuthenticatedUser user) = do
  projectId <- param "project_id"
  requestData <- jsonData >>= parseRequest (builder user projectId)
  maybeProject <- lift $ App.call requestData
  case maybeProject of
    Left App.ProjectNotFound -> status notFound404
    Right project -> status created201 >> (json $ environmentResource project)
