module Api.Handlers.CreateProject
  ( postProjectsR
  ) where

import Api.Import

import qualified Deployments.Domain.Project as Project
import qualified Deployments.UseCases.CreateProject as App

data Request = Request
  { reqProjectName :: !Project.Name
  , reqProjectDeploymentImage :: !Project.DeploymentImage
  , reqProjectSlug :: !Project.Slug
  }

instance FromJSON Request where
  parseJSON =
    withObject "request params" $ \o -> do
      reqProjectName <- o .: "name"
      reqProjectDeploymentImage <- o .: "deployment_image"
      reqProjectSlug <- o .: "slug"
      return Request {..}

mapRequest :: User -> Request -> App.Params
mapRequest User {..} Request {..} = App.Params reqProjectName reqProjectSlug reqProjectDeploymentImage userCompanyId

call :: AuthenticatedUser -> Handler Value
call (AuthenticatedUser user) = do
  requestData <- mapRequest user <$> requireJsonBody
  result <- App.call requestData
  case result of
    Left App.SlugAlreadyExists -> sendResponseStatus conflict409 ()
    Right project -> sendStatusJSON created201 $ toJSON (projectResource project)

postProjectsR :: Handler Value
postProjectsR = userAuth call
