module Api.Handlers.CreateBuild
  ( postBuildsR
  ) where

import Api.Import

import qualified RIO.HashMap as HashMap

import Deployments.UseCases.CreateBuild hiding (call)
import qualified Deployments.UseCases.CreateBuild as App (call)

data Request = Request
  { reqBuildName :: !BuildName
  , reqBuildParams :: !(HashMap Text Text)
  , reqBuildMetadata :: !(Maybe (HashMap Text Text))
  }

instance FromJSON Request where
  parseJSON =
    withObject "request params" $ \o -> do
      reqBuildName <- o .: "name"
      reqBuildParams <- o .: "params"
      reqBuildMetadata <- o .:? "metadata"
      return Request {..}

mapRequest :: Entity Project -> Request -> Params
mapRequest project Request {..} = Params reqBuildName reqBuildParams (fromMaybe HashMap.empty reqBuildMetadata) project

call :: Entity Project -> Handler ()
call project = do
  requestData <- mapRequest project <$> requireJsonBody
  _ <- runAppInHandler $ App.call requestData
  sendResponseStatus created201 ()

postBuildsR :: Handler ()
postBuildsR = projectAuth call
