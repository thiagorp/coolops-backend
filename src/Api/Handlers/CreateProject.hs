module Api.Handlers.CreateProject
  ( postProjectsR
  ) where

import Api.Import

import Deployments.Domain.Project (buildDeploymentImage, buildName, buildSlug)
import qualified Deployments.UseCases.CreateProject as App

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
    projectName = required_ Name reqProjectName |>> buildName
    projectSlug = required_ Slug reqProjectSlug |>> buildSlug
    projectDeploymentImage = required_ DeploymentImage reqProjectDeploymentImage |>> buildDeploymentImage

call :: AuthenticatedUser -> Handler Value
call (AuthenticatedUser user) = do
  requestData <- requireJsonBody >>= parseValidatedRequest (builder user)
  result <- App.call requestData
  case result of
    Left App.SlugAlreadyExists -> sendResponseStatus conflict409 ()
    Right project -> sendStatusJSON created201 $ toJSON (projectResource project)

postProjectsR :: Handler Value
postProjectsR = userAuth call
