module Api.Handlers.CreateDeployment
  ( postDeploymentsR
  ) where

import Api.Import

import Deployments.Database.Build (getBuild)
import Deployments.Database.Environment (getEnvironment)
import Deployments.UseCases.CreateDeployment hiding (call)
import qualified Deployments.UseCases.CreateDeployment as App (call)

data Request = Request
  { reqEnvironmentId :: !UUID
  , reqBuildId :: !UUID
  }

instance FromJSON Request where
  parseJSON =
    withObject "request params" $ \o -> do
      reqEnvironmentId <- o .: "environment_id"
      reqBuildId <- o .: "build_id"
      return Request {..}

mapRequest :: Request -> (UUID, UUID)
mapRequest Request {..} = (reqEnvironmentId, reqBuildId)

findEntities :: User -> (UUID, UUID) -> Handler Params
findEntities User {..} (environmentId, buildId) =
  runDb $ do
    maybeEnvironment <- getEnvironment userCompanyId environmentId
    maybeBuild <- getBuild userCompanyId buildId
    case (,) <$> maybeEnvironment <*> maybeBuild of
      Nothing -> notFound
      Just (environment, build) -> return $ Params build environment

call :: Entity User -> Handler ()
call (Entity _ user) = do
  requestData <- mapRequest <$> requireJsonBody
  appParams <- findEntities user requestData
  result <- App.call appParams
  case result of
    Right _ -> sendResponseStatus created201 ()
    Left ProjectsDontMatch -> sendResponseStatus badRequest400 ()

postDeploymentsR :: Handler ()
postDeploymentsR = userAuth call
