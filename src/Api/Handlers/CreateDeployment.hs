module Api.Handlers.CreateDeployment
  ( postDeploymentsR
  ) where

import Api.Import

import Deployments.Database.Build (getBuild)
import Deployments.Database.Environment (getEnvironment)
import qualified Deployments.UseCases.CreateDeployment as App

data Request = Request
  { reqEnvironmentId :: !Text
  , reqBuildId :: !Text
  }

instance FromJSON Request where
  parseJSON =
    withObject "request params" $ \o -> do
      reqEnvironmentId <- o .: "environment_id"
      reqBuildId <- o .: "build_id"
      return Request {..}

mapRequest :: Request -> (Text, Text)
mapRequest Request {..} = (reqEnvironmentId, reqBuildId)

findEntities :: User -> (Text, Text) -> Handler App.Params
findEntities User {..} (environmentId, buildId) = do
  maybeEnvironment <- getEnvironment userCompanyId environmentId
  maybeBuild <- getBuild userCompanyId buildId
  case (,) <$> maybeEnvironment <*> maybeBuild of
    Nothing -> notFound
    Just (environment, build) -> return $ App.Params build environment

call :: AuthenticatedUser -> Handler ()
call (AuthenticatedUser user) = do
  requestData <- mapRequest <$> requireJsonBody
  appParams <- findEntities user requestData
  result <- App.call appParams
  case result of
    Right _ -> sendResponseStatus created201 ()
    Left App.ProjectsDontMatch -> sendResponseStatus badRequest400 ()

postDeploymentsR :: Handler ()
postDeploymentsR = userAuth call
