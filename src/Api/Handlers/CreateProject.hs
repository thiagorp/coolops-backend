{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Api.Handlers.CreateProject
  ( postProjectsR
  ) where

import Api.Import

import qualified Deployments.UseCases.CreateProject as App

data Request = Request
  { reqProjectName :: !ProjectName
  , reqProjectDeploymentImage :: !DockerImage
  , reqProjectSlug :: !Slug
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

call :: Entity User -> Handler Value
call (Entity _ user) = do
  requestData <- mapRequest user <$> requireCheckJsonBody
  result <- runAppInHandler $ App.call requestData
  case result of
    Left App.SlugAlreadyExists -> sendResponseStatus conflict409 ()
    Right project -> sendStatusJSON created201 $ toJSON (projectResource project)

postProjectsR :: Handler Value
postProjectsR = userAuth call
