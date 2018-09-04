module Handlers.CreateOnboardingProject
  ( call
  ) where

import RIO

import Data.Aeson hiding (json)
import Network.HTTP.Types.Status (created201)
import Web.Scotty.Trans

import Authorization (AuthenticatedUser(..), User(..))
import Deployments.Domain.Project (buildName)
import qualified Deployments.UseCases.CreateOnboardingProject as App
import Types
import Validation

data Field =
  Name

instance HasFieldName Field where
  fieldName field =
    case field of
      Name -> "name"

newtype Request = Request
  { reqProjectName :: Maybe Text
  }

instance FromJSON Request where
  parseJSON =
    withObject "request params" $ \o -> do
      reqProjectName <- o .:? fieldName Name
      return Request {..}

builder :: User -> Request -> WebValidation App.Params
builder User {..} Request {..} =
  App.Params <$> projectName <*> pure userCompanyId
  where
    projectName = required Name reqProjectName |>> buildName

call :: AuthenticatedUser -> WebMonad ()
call (AuthenticatedUser user) = do
  requestData <- jsonData >>= parseRequest (builder user)
  _ <- lift $ App.call requestData
  status created201
