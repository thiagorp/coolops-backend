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
  { slackAccessTokenTeamName :: !Text
  , slackAccessTokenTeamId :: !Text
  , slackAccessTokenScopes :: !Text
  , slackAccessTokenUserAccessToken :: !Text
  , slackAccessTokenBotAccessToken :: !Text
  , slackAccessTokenBotUserId :: !Text
  }

instance FromJSON OAuthTokenResponse where
  parseJSON =
    withObject "response" $ \o -> do
      slackAccessTokenUserAccessToken <- o .: "access_token"
      slackAccessTokenScopes <- o .: "scope"
      slackAccessTokenTeamName <- o .: "team_name"
      slackAccessTokenTeamId <- o .: "team_id"
      botO <- o .: "bot"
      slackAccessTokenBotUserId <- botO .: "bot_user_id"
      slackAccessTokenBotAccessToken <- botO .: "bot_access_token"
      return OAuthTokenResponse {..}

data SlackClientError
  = WrongBodyError Text
                   LBS.ByteString
  | UnexpectedHttpStatusError Int
  deriving (Show)

parseResponse :: FromJSON a => SlackResponse -> Either SlackClientError a
parseResponse response =
  case statusCode (responseStatus response) of
    200 ->
      case eitherDecode (responseBody response) of
        Left err -> Left $ WrongBodyError (Text.pack err) (responseBody response)
        Right r -> Right r
    _ -> Left $ UnexpectedHttpStatusError $ statusCode $ responseStatus response

getToken :: (SlackClientMonad m) => Text -> m (Either SlackClientError OAuthTokenResponse)
getToken code = do
  response <- slackRequest (GetOAuthToken $ Text.encodeUtf8 code)
  return $ parseResponse response

revokeToken :: (SlackClientMonad m) => Text -> m (Either SlackClientError ())
revokeToken token = do
  response <- slackRequest (RevokeToken $ Text.encodeUtf8 token)
  case statusCode (responseStatus response) of
    200 -> return $ Right ()
    _ -> return $ Left $ UnexpectedHttpStatusError $ statusCode $responseStatus response
