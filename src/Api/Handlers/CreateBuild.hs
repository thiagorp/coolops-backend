module Api.Handlers.CreateBuild
  ( postBuildsR
  ) where

import Api.Import

import qualified RIO.HashMap as HashMap

import Deployments.Domain.Build (Name)
import qualified Deployments.UseCases.CreateBuild as App

data Request = Request
  { reqBuildName :: !Name
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

mapRequest :: Project -> Request -> App.Params
mapRequest project Request {..} =
  App.Params reqBuildName reqBuildParams (fromMaybe HashMap.empty reqBuildMetadata) project

call :: AuthenticatedProject -> Handler ()
call (AuthenticatedProject project) = do
  requestData <- mapRequest project <$> requireJsonBody
  _ <- App.call requestData
  sendResponseStatus created201 ()

postBuildsR :: Handler ()
postBuildsR = projectAuth call
