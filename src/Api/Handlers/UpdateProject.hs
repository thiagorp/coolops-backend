module Api.Handlers.UpdateProject
  ( patchUpdateProjectR
  ) where

import Api.Import

import Deployments.Database.Project (getProject)
import Deployments.Domain.Project (Project, buildDeploymentImage, buildName, buildSlug)
import qualified Deployments.UseCases.UpdateProject as App

data Fields
  = Name
  | DeploymentImage
  | Slug

instance HasFieldName Fields where
  fieldName field =
    case field of
      Name -> "name"
      Slug -> "slug"
      DeploymentImage -> "deployment_image"

data Request = Request
  { reqProjectName :: !(Maybe Text)
  , reqProjectSlug :: !(Maybe Text)
  , reqProjectDeploymentImage :: !(Maybe Text)
  }

instance FromJSON Request where
  parseJSON =
    withObject "request params" $ \o -> do
      reqProjectName <- o .:? fieldName Name
      reqProjectSlug <- o .:? fieldName Slug
      reqProjectDeploymentImage <- o .:? fieldName DeploymentImage
      return Request {..}

builder :: Request -> WebValidation App.Params
builder Request {..} = App.Params <$> projectName <*> projectSlug <*> projectDeploymentImage
  where
    projectName = required_ Name reqProjectName |>> buildName
    projectSlug = required_ Slug reqProjectSlug |>> buildSlug
    projectDeploymentImage = required_ DeploymentImage reqProjectDeploymentImage |>> buildDeploymentImage

update :: Project -> Handler ProjectResource
update project = do
  requestData <- requireJsonBody >>= parseValidatedRequest builder
  updatedProject <- App.call project requestData
  return $ projectResource updatedProject

call :: Text -> AuthenticatedUser -> Handler Value
call projectId (AuthenticatedUser user) = do
  maybeProject <- getProject (userCompanyId user) projectId
  case maybeProject of
    Just project -> toJSON <$> update project
    Nothing -> notFound

patchUpdateProjectR :: Text -> Handler Value
patchUpdateProjectR = userAuth . call
