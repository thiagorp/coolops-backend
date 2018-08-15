module Handlers.CreateBuild
  ( call
  ) where

import RIO

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

instance HasFieldName Fields where
  fieldName field =
    case field of
      Name -> "name"
      Params -> "params"

data Request = Request
  { reqBuildName :: !(Maybe Text)
  , reqBuildParams :: !(Maybe (HashMap Text Text))
  }

instance FromJSON Request where
  parseJSON =
    withObject "request params" $ \o -> do
      reqBuildName <- o .:? fieldName Name
      reqBuildParams <- o .:? fieldName Params
      return Request {..}

builder :: Project -> Request -> WebValidation App.Params
builder project Request {..} =
  App.Params <$> buildName <*> buildParams <*> pure project
  where
    buildName = required Name reqBuildName >>> buildBuildName
    buildParams = required Params reqBuildParams >>> valid

call :: AuthenticatedProject -> WebMonad ()
call (AuthenticatedProject project) = do
  requestData <- jsonData >>= parseRequest (builder project)
  _ <- lift $ App.call requestData
  status created201
