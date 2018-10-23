module Api.Handlers.UpdateProject
  ( patchUpdateProjectR
  ) where

import Api.Import

import Deployments.Database.Project (getProject)
import qualified Deployments.Domain.Project as Project
import qualified Deployments.UseCases.UpdateProject as App

data Request = Request
  { reqProjectName :: !Project.Name
  , reqProjectSlug :: !Project.Slug
  , reqProjectDeploymentImage :: !Project.DeploymentImage
  }

instance FromJSON Request where
  parseJSON =
    withObject "request params" $ \o -> do
      reqProjectName <- o .: "name"
      reqProjectSlug <- o .: "slug"
      reqProjectDeploymentImage <- o .: "deployment_image"
      return Request {..}

mapRequest :: Request -> App.Params
mapRequest Request {..} = App.Params reqProjectName reqProjectSlug reqProjectDeploymentImage

update :: Project -> Handler ProjectResource
update project = do
  requestData <- mapRequest <$> requireJsonBody
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
