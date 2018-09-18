module Handlers.CreateProject
  ( call
  ) where

import RIO

import Data.Aeson hiding (json)
import Network.HTTP.Types.Status (conflict409, created201)
import Web.Scotty.Trans

import Authorization (AuthenticatedUser(..), User(..))
import Deployments.Domain.Project (buildDeploymentImage, buildName, buildSlug)
import qualified Deployments.UseCases.CreateProject as App
import Resources
import Types
import Validation

data Fields
  = Name
  | DeploymentImage
  | Slug

instance HasFieldName Fields where
  fieldName field =
    case field of
      Name -> "name"
      DeploymentImage -> "deployment_image"
      Slug -> "slug"

data Request = Request
  { reqProjectName :: !(Maybe Text)
  , reqProjectDeploymentImage :: !(Maybe Text)
  , reqProjectSlug :: !(Maybe Text)
  }

instance FromJSON Request where
  parseJSON =
    withObject "request params" $ \o -> do
      reqProjectName <- o .:? fieldName Name
      reqProjectDeploymentImage <- o .:? fieldName DeploymentImage
      reqProjectSlug <- o .:? fieldName Slug
      return Request {..}

builder :: User -> Request -> WebValidation App.Params
builder User {..} Request {..} =
  App.Params <$> projectName <*> projectSlug <*> projectDeploymentImage <*> pure userCompanyId
  where
    projectName = required Name reqProjectName |>> buildName
    projectSlug = required Slug reqProjectSlug |>> buildSlug
    projectDeploymentImage = required DeploymentImage reqProjectDeploymentImage |>> buildDeploymentImage

call :: AuthenticatedUser -> WebMonad ()
call (AuthenticatedUser user) = do
  requestData <- jsonData >>= parseRequest (builder user)
  result <- lift $ App.call requestData
  case result of
    Left App.SlugAlreadyExists -> status conflict409
    Right project -> do
      status created201
      json $ projectResource project
