module Handlers.CreateProject
  ( call
  ) where

import RIO

import Data.Aeson hiding (json)
import Network.HTTP.Types.Status (created201)
import Web.Scotty.Trans

import Authorization (AuthenticatedUser(..), User(..))
import Deployments.Domain (buildProjectDeploymentImage, buildProjectName)
import qualified Deployments.UseCases.CreateProject as App
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

builder :: User -> Request -> WebValidation App.Params
builder (User {..}) (Request {..}) =
  App.Params <$> projectName <*> projectDeploymentImage <*> (pure userCompanyId)
  where
    projectName = required Name reqProjectName >>> buildProjectName
    projectDeploymentImage =
      required DeploymentImage reqProjectDeploymentImage >>>
      buildProjectDeploymentImage

call :: AuthenticatedUser -> WebMonad ()
call (AuthenticatedUser user) = do
  requestData <- jsonData >>= parseRequest (builder user)
  project <- lift $ App.call requestData
  status created201
  json $ projectResource project
