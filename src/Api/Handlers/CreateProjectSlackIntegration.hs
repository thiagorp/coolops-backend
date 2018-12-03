module Api.Handlers.CreateProjectSlackIntegration
  ( postCreateProjectSlackIntegrationR
  ) where

import Api.Import

import qualified Deployments.Database.Project as DB
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

buildParams :: App.Entity App.Project -> Request -> App.Params
buildParams (App.Entity projectId _) Request {..} =
  App.Params {paramChannelId = reqChannelId, paramChannelName = reqChannelName, paramProjectId = projectId}

call :: App.UUID -> App.Entity App.User -> Handler ()
call pId (App.Entity _ App.User {..}) = do
  maybeProject <- DB.runDb $ DB.getProject userCompanyId pId
  case maybeProject of
    Nothing -> sendResponseStatus notFound404 ()
    Just project -> do
      request <- requireJsonBody
      void $ App.call (buildParams project request)

postCreateProjectSlackIntegrationR :: App.UUID -> Handler ()
postCreateProjectSlackIntegrationR projectId = userAuth (call projectId)