module Api.Handlers.UpdateProject
  ( patchUpdateProjectR
  ) where

import Api.Import

import Deployments.Database.Project (getProject, runDb)
import qualified Deployments.UseCases.UpdateProject as App

data Request = Request
  { reqProjectName :: !App.ProjectName
  , reqProjectSlug :: !App.Slug
  , reqProjectDeploymentImage :: !App.DockerImage
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

update :: App.Entity App.Project -> Handler ()
update (App.Entity projectId _) = do
  requestData <- mapRequest <$> requireJsonBody
  App.call projectId requestData

call :: App.UUID -> App.Entity App.User -> Handler ()
call projectId (App.Entity _ user) = do
  maybeProject <- runDb $ getProject (App.userCompanyId user) projectId
  case maybeProject of
    Just project -> update project
    Nothing -> notFound

patchUpdateProjectR :: App.UUID -> Handler ()
patchUpdateProjectR = userAuth . call
