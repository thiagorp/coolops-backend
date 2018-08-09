module Slack.Api.OAuth
  ( OAuthTokenResponse(..)
  , SlackClientMonad
  , SlackClientError(..)
  , WorkspaceTokenResponse(..)
  , getToken
  , getWorkspaceToken
  , revokeToken
  ) where

import RIO
import qualified RIO.ByteString.Lazy as LBS
import qualified RIO.Text as Text

import Data.Aeson
import Network.HTTP.Client
import Network.HTTP.Types

import Slack.Api.ClientBase

data OAuthTokenResponse = OAuthTokenResponse
  { teamName :: !Text
  , teamSlackId :: !Text
  , teamAccessToken :: !Text
  , incomingWebhookUrl :: !Text
  , incomingWebhookChannel :: !Text
  , incomingWebhookConfigurationUrl :: !Text
  , botUserId :: !Text
  , botUserAccessToken :: !Text
  }

instance FromJSON OAuthTokenResponse where
  parseJSON =
    withObject "response" $ \o -> do
      teamAccessToken <- o .: "access_token"
      teamName <- o .: "team_name"
      teamSlackId <- o .: "team_id"
      iwO <- o .: "incoming_webhook"
      incomingWebhookUrl <- iwO .: "url"
      incomingWebhookChannel <- iwO .: "channel_id"
      incomingWebhookConfigurationUrl <- iwO .: "configuration_url"
      botO <- o .: "bot"
      botUserId <- botO .: "bot_user_id"
      botUserAccessToken <- botO .: "bot_access_token"
      return OAuthTokenResponse {..}

data WorkspaceTokenResponse = WorkspaceTokenResponse
  { tokenAccessToken :: !Text
  , tokenWorkspaceName :: !Text
  , tokenAppId :: !Text
  , tokenAppUserId :: !Text
  , tokenInstallerUserId :: !Text
  , tokenAuthorizingUserId :: !Text
  , tokenTeamId :: !Text
  , tokenChannelId :: !Text
  , tokenScopes :: !Value
  }

instance FromJSON WorkspaceTokenResponse where
  parseJSON =
    withObject "response" $ \o -> do
      tokenAccessToken <- o .: "access_token"
      tokenWorkspaceName <- o .: "team_name"
      tokenAppId <- o .: "app_id"
      tokenAppUserId <- o .: "app_user_id"
      installerUser <- o .: "installer_user"
      tokenInstallerUserId <- installerUser .: "user_id"
      authorizingUser <- o .: "authorizing_user"
      tokenAuthorizingUserId <- authorizingUser .: "user_id"
      tokenTeamId <- o .: "team_id"
      tokenChannelId <- o .: "single_channel_id"
      tokenScopes <- o .: "scopes"
      return WorkspaceTokenResponse {..}

data SlackClientError
  = WrongBodyError Text
                   LBS.ByteString
  | UnexpectedHttpStatusError Int
  deriving (Show)

getWorkspaceToken ::
     (SlackClientMonad m)
  => Text
  -> Text
  -> m (Either SlackClientError WorkspaceTokenResponse)
getWorkspaceToken code redirectUri = do
  response <-
    slackRequest
      (GetWorkspaceToken (Text.encodeUtf8 redirectUri) (Text.encodeUtf8 code))
  return $ parseResponse response

parseResponse :: FromJSON a => SlackResponse -> Either SlackClientError a
parseResponse response =
  case statusCode (responseStatus response) of
    200 ->
      case eitherDecode (responseBody response) of
        Left err ->
          Left $ WrongBodyError (Text.pack err) (responseBody response)
        Right r -> Right r
    _ -> Left $ UnexpectedHttpStatusError $ statusCode $ responseStatus response

getToken ::
     (SlackClientMonad m)
  => Text
  -> m (Either SlackClientError OAuthTokenResponse)
getToken code = do
  response <- slackRequest (GetOAuthToken $ Text.encodeUtf8 code)
  return $ parseResponse response

revokeToken :: (SlackClientMonad m) => Text -> m (Either SlackClientError ())
revokeToken token = do
  response <- slackRequest (RevokeToken $ Text.encodeUtf8 token)
  case statusCode (responseStatus response) of
    200 -> return $ Right ()
    _ ->
      return $
      Left $ UnexpectedHttpStatusError $ statusCode $responseStatus response
