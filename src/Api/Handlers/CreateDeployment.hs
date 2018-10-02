module Api.Handlers.CreateDeployment
  ( postDeploymentsR
  ) where

import Api.Import

import Deployments.Database.Build (getBuild)
import Deployments.Database.Environment (getEnvironment)
import qualified Deployments.UseCases.CreateDeployment as App

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
    environmentId = required_ EnvironmentID reqEnvironmentId |>> valid
    buildId = required_ BuildID reqBuildId |>> valid

findEntities :: User -> (Text, Text) -> Handler App.Params
findEntities User {..} (environmentId, buildId) = do
  maybeEnvironment <- getEnvironment userCompanyId environmentId
  maybeBuild <- getBuild userCompanyId buildId
  case (,) <$> maybeEnvironment <*> maybeBuild of
    Nothing -> notFound
    Just (environment, build) -> return $ App.Params build environment

call :: AuthenticatedUser -> Handler ()
call (AuthenticatedUser user) = do
  requestData <- requireJsonBody >>= parseValidatedRequest builder >>= findEntities user
  result <- App.call requestData
  case result of
    Right _ -> sendResponseStatus created201 ()
    Left App.ProjectsDontMatch -> sendResponseStatus badRequest400 ()

postDeploymentsR :: Handler ()
postDeploymentsR = userAuth call
