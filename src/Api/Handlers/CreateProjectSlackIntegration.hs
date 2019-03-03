{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

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

buildParams' :: Entity Project -> Request -> App.Params
buildParams' (Entity projectId _) Request {..} =
  App.Params
    { paramChannelId = reqChannelId
    , paramChannelName = reqChannelName
    , paramProjectId = projectId
    }

call :: UUID -> Entity User -> Handler ()
call pId (Entity _ User {..}) = do
  maybeProject <- runAppInHandler $ DB.getProject userCompanyId pId
  case maybeProject of
    Nothing -> sendResponseStatus notFound404 ()
    Just project -> do
      request <- requireJsonBody
      void $ runAppInHandler $ App.call (buildParams' project request)

postCreateProjectSlackIntegrationR :: UUID -> Handler ()
postCreateProjectSlackIntegrationR projectId = userAuth (call projectId)
