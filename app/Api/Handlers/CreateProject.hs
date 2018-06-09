module Handlers.CreateProject
  ( call
  ) where

import RIO

import Data.Aeson hiding (json)
import Web.Scotty.Trans

import Authorization (AuthenticatedUser(..), User(..))
import Deployments.Domain (buildProjectName)
import qualified Deployments.UseCases.CreateProject as App
import Resources
import Types
import Validation

data Fields =
  Name

instance HasFieldName Fields where
  fieldName field =
    case field of
      Name -> "name"

data Request = Request
  { reqProjectName :: !(Maybe Text)
  }

instance FromJSON Request where
  parseJSON =
    withObject "request params" $ \o -> do
      reqProjectName <- o .:? (fieldName Name)
      return Request {..}

builder :: User -> Request -> WebValidation App.CreateProject
builder (User {..}) (Request {..}) =
  App.CreateProject <$> projectName <*> (pure userCompanyId)
  where
    projectName = required Name reqProjectName >>> buildProjectName

call :: AuthenticatedUser -> WebMonad ()
call (AuthenticatedUser user) = do
  requestData <- jsonData >>= parseRequest (builder user)
  project <- lift $ App.call requestData
  json $ projectResource project
