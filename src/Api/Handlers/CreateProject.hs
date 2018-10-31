module Api.Handlers.CreateProject
  ( postProjectsR
  ) where

import Api.Import

import qualified Deployments.UseCases.CreateProject as App

data Request = Request
  { reqProjectName :: !App.ProjectName
  , reqProjectDeploymentImage :: !App.DockerImage
  , reqProjectSlug :: !App.Slug
  }

instance FromJSON Request where
  parseJSON =
    withObject "request params" $ \o -> do
      reqProjectName <- o .: "name"
      reqProjectDeploymentImage <- o .: "deployment_image"
      reqProjectSlug <- o .: "slug"
      return Request {..}

mapRequest :: App.User -> Request -> App.Params
mapRequest App.User {..} Request {..} = App.Params reqProjectName reqProjectSlug reqProjectDeploymentImage userCompanyId

call :: App.Entity App.User -> Handler Value
call (App.Entity _ user) = do
  requestData <- mapRequest user <$> requireJsonBody
  result <- App.runDb $ App.call requestData
  case result of
    Left App.SlugAlreadyExists -> sendResponseStatus conflict409 ()
    Right project -> sendStatusJSON created201 $ toJSON (projectResource project)

postProjectsR :: Handler Value
postProjectsR = userAuth call
