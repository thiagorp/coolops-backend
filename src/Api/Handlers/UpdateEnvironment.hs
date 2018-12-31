module Api.Handlers.UpdateEnvironment
  ( patchUpdateEnvironmentR
  ) where

import Api.Import

import Deployments.Database.Environment (getEnvironment)
import qualified Deployments.UseCases.UpdateEnvironment as App

data Request = Request
  { reqEnvironmentName :: !EnvironmentName
  , reqEnvironmentSlug :: !Slug
  , reqEnvironmentEnvVars :: !(HashMap Text Text)
  }

instance FromJSON Request where
  parseJSON =
    withObject "request params" $ \o -> do
      reqEnvironmentName <- o .: "name"
      reqEnvironmentSlug <- o .: "slug"
      reqEnvironmentEnvVars <- o .: "env_vars"
      return Request {..}

mapRequest :: Request -> App.Params
mapRequest Request {..} = App.Params reqEnvironmentName reqEnvironmentSlug reqEnvironmentEnvVars

call_ :: Entity Environment -> Handler ()
call_ (Entity environmentId _) = do
  requestData <- mapRequest <$> requireJsonBody
  runAppInHandler $ App.call environmentId requestData

call :: UUID -> Entity User -> Handler ()
call environmentId (Entity _ User {..}) = do
  maybeEnvironment <- runAppInHandler $ getEnvironment userCompanyId environmentId
  case maybeEnvironment of
    Just environment -> call_ environment
    Nothing -> notFound

patchUpdateEnvironmentR :: UUID -> Handler ()
patchUpdateEnvironmentR = userAuth . call
