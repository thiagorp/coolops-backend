module Slack.Api.OAuth
  ( OAuthTokenResponse(..)
  , SlackClientMonad
  , SlackClientError(..)
  , getToken
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

data SlackClientError
  = WrongBodyError LBS.ByteString
  | UnexpectedHttpStatusError Int
  deriving (Show)

getToken ::
     (SlackClientMonad m)
  => Text
  -> m (Either SlackClientError OAuthTokenResponse)
getToken code = do
  response <- slackRequest (GetOAuthToken $ Text.encodeUtf8 code)
  case statusCode (responseStatus response) of
    200 ->
      case decode (responseBody response) of
        Nothing -> return $ Left $ WrongBodyError $ responseBody response
        Just r -> return $ Right r
    _ ->
      return $
      Left $ UnexpectedHttpStatusError $ statusCode $ responseStatus response

revokeToken :: (SlackClientMonad m) => Text -> m (Either SlackClientError ())
revokeToken token = do
  response <- slackRequest (RevokeToken $ Text.encodeUtf8 token)
  case statusCode (responseStatus response) of
    200 -> return $ Right ()
    _ ->
      return $
      Left $ UnexpectedHttpStatusError $ statusCode $responseStatus response
