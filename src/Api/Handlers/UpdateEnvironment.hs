module Api.Handlers.UpdateEnvironment
  ( patchUpdateEnvironmentR
  ) where

import Api.Import

import Deployments.Database.Environment (getEnvironment)
import qualified Deployments.Domain.Environment as Environment
import qualified Deployments.UseCases.UpdateEnvironment as App

data Request = Request
  { reqEnvironmentName :: !Environment.Name
  , reqEnvironmentSlug :: !Environment.Slug
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

update :: Environment.Environment -> Handler EnvironmentResource
update environment = do
  requestData <- mapRequest <$> requireJsonBody
  updatedEnvironment <- App.call environment requestData
  return $ environmentResource updatedEnvironment

call :: Text -> AuthenticatedUser -> Handler Value
call environmentId (AuthenticatedUser user) = do
  maybeEnvironment <- getEnvironment (userCompanyId user) environmentId
  case maybeEnvironment of
    Just environment -> toJSON <$> update environment
    Nothing -> notFound

patchUpdateEnvironmentR :: Text -> Handler Value
patchUpdateEnvironmentR = userAuth . call
