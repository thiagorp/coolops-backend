module Api.Handlers.CreateEnvironment
  ( postProjectsEnvironmentsR
  ) where

import Api.Import

import qualified Deployments.UseCases.CreateEnvironment as App

data Request = Request
  { reqEnvironmentName :: !App.EnvironmentName
  , reqEnvironmentEnvVars :: !(HashMap Text Text)
  , reqEnvironmentSlug :: !App.Slug
  }

instance FromJSON Request where
  parseJSON =
    withObject "request params" $ \o -> do
      reqEnvironmentName <- o .: "name"
      reqEnvironmentEnvVars <- o .: "env_vars"
      reqEnvironmentSlug <- o .: "slug"
      return Request {..}

mapRequest :: Request -> App.Params
mapRequest Request {..} = App.Params reqEnvironmentName reqEnvironmentEnvVars reqEnvironmentSlug

call :: App.UUID -> App.Entity App.User -> Handler Value
call projectId (App.Entity _ App.User {..}) = do
  requestData <- mapRequest <$> requireJsonBody
  maybeProject <- App.call projectId userCompanyId requestData
  case maybeProject of
    Left App.ProjectNotFound -> notFound
    Left App.SlugAlreadyExists -> sendResponseStatus conflict409 ()
    Right project -> sendStatusJSON created201 $ toJSON (environmentResource project)

postProjectsEnvironmentsR :: App.UUID -> Handler Value
postProjectsEnvironmentsR projectId = userAuth (call projectId)
