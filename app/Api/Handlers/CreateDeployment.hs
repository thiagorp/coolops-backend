module Handlers.CreateDeployment
  ( call
  ) where

import RIO

import Data.Aeson hiding (json)
import Network.HTTP.Types.Status (badRequest400, created201, notFound404)
import Web.Scotty.Trans

import Authorization (AuthenticatedUser(..), User(..))
import Deployments.Classes (getBuild, getEnvironment)
import qualified Deployments.UseCases.CreateDeployment as App
import Types
import Validation

data Fields
  = EnvironmentID
  | BuildID

instance HasFieldName Fields where
  fieldName field =
    case field of
      EnvironmentID -> "environment_id"
      BuildID -> "build_id"

data Request = Request
  { reqEnvironmentId :: !(Maybe Text)
  , reqBuildId :: !(Maybe Text)
  }

instance FromJSON Request where
  parseJSON =
    withObject "request params" $ \o -> do
      reqEnvironmentId <- o .:? fieldName EnvironmentID
      reqBuildId <- o .:? fieldName BuildID
      return Request {..}

builder :: Request -> WebValidation (Text, Text)
builder Request {..} = (,) <$> environmentId <*> buildId
  where
    environmentId = required EnvironmentID reqEnvironmentId >>> valid
    buildId = required BuildID reqBuildId >>> valid

findEntities :: User -> (Text, Text) -> WebMonad App.Params
findEntities User {..} (environmentId, buildId) = do
  maybeEnvironment <- lift $ getEnvironment userCompanyId environmentId
  maybeBuild <- lift $ getBuild userCompanyId buildId
  case (,) <$> maybeEnvironment <*> maybeBuild of
    Nothing -> status notFound404 >> finish
    Just (environment, build) -> return $ App.Params build environment

call :: AuthenticatedUser -> WebMonad ()
call (AuthenticatedUser user) = do
  requestData <- jsonData >>= parseRequest builder >>= findEntities user
  result <- lift $ App.call requestData
  case result of
    Right _ -> status created201
    Left App.ProjectsDontMatch -> status badRequest400
