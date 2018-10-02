module Api.Handlers.CreateBuild
  ( postBuildsR
  ) where

import Api.Import

import qualified RIO.HashMap as HashMap

import Deployments.Domain.Build (buildBuildName)
import qualified Deployments.UseCases.CreateBuild as App

data Fields
  = Name
  | Params
  | Metadata

instance HasFieldName Fields where
  fieldName field =
    case field of
      Name -> "name"
      Params -> "params"
      Metadata -> "metadata"

data Request = Request
  { reqBuildName :: !(Maybe Text)
  , reqBuildParams :: !(Maybe (HashMap Text Text))
  , reqBuildMetadata :: !(Maybe (HashMap Text Text))
  }

instance FromJSON Request where
  parseJSON =
    withObject "request params" $ \o -> do
      reqBuildName <- o .:? fieldName Name
      reqBuildParams <- o .:? fieldName Params
      reqBuildMetadata <- o .:? fieldName Metadata
      return Request {..}

builder :: Project -> Request -> WebValidation App.Params
builder project Request {..} = App.Params <$> buildName <*> buildParams <*> buildMetadata <*> pure project
  where
    buildName = required_ Name reqBuildName |>> buildBuildName
    buildParams = required_ Params reqBuildParams |>> valid
    buildMetadata = optional_ Metadata reqBuildMetadata |>> withDefault HashMap.empty

call :: AuthenticatedProject -> Handler ()
call (AuthenticatedProject project) = do
  requestData <- requireJsonBody >>= parseValidatedRequest (builder project)
  _ <- App.call requestData
  sendResponseStatus created201 ()

postBuildsR :: Handler ()
postBuildsR = projectAuth call
