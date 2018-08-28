module Handlers.CreateBuild
  ( call
  ) where

import RIO hiding (optional)
import qualified RIO.HashMap as HashMap

import Data.Aeson hiding (json)
import Network.HTTP.Types.Status (created201)
import Web.Scotty.Trans

import Authorization (AuthenticatedProject(..), Project(..))
import Deployments.Domain.Build (buildBuildName)
import qualified Deployments.UseCases.CreateBuild as App
import Types
import Validation

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
builder project Request {..} =
  App.Params <$> buildName <*> buildParams <*> buildMetadata <*> pure project
  where
    buildName = required Name reqBuildName |>> buildBuildName
    buildParams = required Params reqBuildParams |>> valid
    buildMetadata =
      optional Metadata reqBuildMetadata |>> withDefault HashMap.empty

call :: AuthenticatedProject -> WebMonad ()
call (AuthenticatedProject project) = do
  requestData <- jsonData >>= parseRequest (builder project)
  _ <- lift $ App.call requestData
  status created201
