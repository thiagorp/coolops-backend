module Handlers.UpdateProject
  ( call
  ) where

import RIO

import Data.Aeson hiding (json)
import Network.HTTP.Types.Status (notFound404)
import Web.Scotty.Trans

import Authorization (AuthenticatedUser(..), User(..))
import Deployments.Classes (getProject)
import Deployments.Domain (Project, buildProjectName)
import qualified Deployments.UseCases.UpdateProject as App
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

builder :: Request -> WebValidation App.UpdateProject
builder Request {..} = App.UpdateProject <$> projectName
  where
    projectName = required Name reqProjectName >>> buildProjectName

update :: Project -> WebMonad ()
update project = do
  requestData <- jsonData >>= parseRequest builder
  updatedProject <- lift $ App.call project requestData
  json $ projectResource updatedProject

call :: AuthenticatedUser -> WebMonad ()
call (AuthenticatedUser user) = do
  projectId <- param "id"
  maybeProject <- lift $ getProject (userCompanyId user) projectId
  case maybeProject of
    Just project -> update project
    Nothing -> status notFound404
