module Handlers.UpdateProject
  ( call
  ) where

import RIO

import Data.Aeson hiding (json)
import Network.HTTP.Types.Status (notFound404)
import Web.Scotty.Trans

import Authorization (AuthenticatedUser(..), User(..))
import Deployments.Classes (getProject)
import Deployments.Domain
  ( Project
  , buildProjectDeploymentImage
  , buildProjectName
  )
import qualified Deployments.UseCases.UpdateProject as App
import Resources
import Types
import Validation

data Fields
  = Name
  | DeploymentImage

instance HasFieldName Fields where
  fieldName field =
    case field of
      Name -> "name"
      DeploymentImage -> "deployment_image"

data Request = Request
  { reqProjectName :: !(Maybe Text)
  , reqProjectDeploymentImage :: !(Maybe Text)
  }

instance FromJSON Request where
  parseJSON =
    withObject "request params" $ \o -> do
      reqProjectName <- o .:? (fieldName Name)
      reqProjectDeploymentImage <- o .:? (fieldName DeploymentImage)
      return Request {..}

builder :: Request -> WebValidation App.Params
builder Request {..} = App.Params <$> projectName <*> projectDeploymentImage
  where
    projectName = required Name reqProjectName >>> buildProjectName
    projectDeploymentImage =
      required DeploymentImage reqProjectDeploymentImage >>>
      buildProjectDeploymentImage

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
