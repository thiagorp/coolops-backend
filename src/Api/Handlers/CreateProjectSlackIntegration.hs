module Api.Handlers.CreateProjectSlackIntegration
  ( postCreateProjectSlackIntegrationR
  ) where

import Api.Import

import qualified Deployments.Database.Project as ProjectsDB
import qualified Slack.UseCases.CreateProjectIntegration as App

data Request = Request
  { reqChannelName :: !Text
  , reqChannelId :: !Text
  }

instance FromJSON Request where
  parseJSON =
    withObject "request params" $ \o -> do
      reqChannelName <- o .: "channel_name"
      reqChannelId <- o .: "channel_id"
      return Request {..}

buildParams :: App.ProjectID -> Request -> App.Params
buildParams projectId Request {..} =
  App.Params
    {integrationChannelId = reqChannelId, integrationChannelName = reqChannelName, integrationProjectId = projectId}

call :: Text -> AuthenticatedUser -> Handler ()
call pId (AuthenticatedUser user) = do
  maybeProject <- ProjectsDB.getProject (userCompanyId user) pId
  case maybeProject of
    Nothing -> sendResponseStatus notFound404 ()
    Just Project {..} -> do
      request <- requireJsonBody
      void $ App.call (buildParams projectId request)

postCreateProjectSlackIntegrationR :: Text -> Handler ()
postCreateProjectSlackIntegrationR projectId = userAuth (call projectId)
