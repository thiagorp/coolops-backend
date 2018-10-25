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
  { tokenTeamName :: !Text
  , tokenTeamId :: !Text
  , tokenScopes :: !Text
  , tokenUserAccessToken :: !Text
  , tokenBotAccessToken :: !Text
  , tokenBotUserId :: !Text
  }

instance FromJSON OAuthTokenResponse where
  parseJSON =
    withObject "response" $ \o -> do
      tokenUserAccessToken <- o .: "access_token"
      tokenScopes <- o .: "scope"
      tokenTeamName <- o .: "team_name"
      tokenTeamId <- o .: "team_id"
      botO <- o .: "bot"
      tokenBotUserId <- botO .: "bot_user_id"
      tokenBotAccessToken <- botO .: "bot_access_token"
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
