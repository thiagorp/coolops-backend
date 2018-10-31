module Api.Handlers.UpdateEnvironment
  ( patchUpdateEnvironmentR
  ) where

import Api.Import

import Deployments.Database.Environment (getEnvironment, runDb)
import qualified Deployments.UseCases.UpdateEnvironment as App

data Request = Request
  { reqEnvironmentName :: !App.EnvironmentName
  , reqEnvironmentSlug :: !App.Slug
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

update :: App.Entity App.Environment -> Handler ()
update (App.Entity environmentId _) = do
  requestData <- mapRequest <$> requireJsonBody
  App.call environmentId requestData

call :: App.UUID -> App.Entity App.User -> Handler ()
call environmentId (App.Entity _ App.User {..}) = do
  maybeEnvironment <- runDb $ getEnvironment userCompanyId environmentId
  case maybeEnvironment of
    Just environment -> update environment
    Nothing -> notFound

patchUpdateEnvironmentR :: App.UUID -> Handler ()
patchUpdateEnvironmentR = userAuth . call
