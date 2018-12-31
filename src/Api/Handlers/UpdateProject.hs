module Api.Handlers.UpdateProject
  ( patchUpdateProjectR
  ) where

import Api.Import

import Deployments.Database.Project (getProject)
import qualified Deployments.UseCases.UpdateProject as App

data Request = Request
  { reqProjectName :: !ProjectName
  , reqProjectSlug :: !Slug
  , reqProjectDeploymentImage :: !DockerImage
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

call_ :: Entity Project -> Handler ()
call_ (Entity projectId _) = do
  requestData <- mapRequest <$> requireJsonBody
  runAppInHandler $ App.call projectId requestData

call :: UUID -> Entity User -> Handler ()
call projectId (Entity _ user) = do
  maybeProject <- runAppInHandler $ getProject (userCompanyId user) projectId
  case maybeProject of
    Just project -> call_ project
    Nothing -> notFound

patchUpdateProjectR :: UUID -> Handler ()
patchUpdateProjectR = userAuth . call
