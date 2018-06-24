module Slack.Api.ClientBase
  ( Action(..)
  , SlackClientMonad
  , slackRequest
  ) where

import RIO
import qualified RIO.ByteString.Lazy as LBS
import qualified RIO.Text as Text

import Network.HTTP.Client

import Http.Classes
import Slack.Api.Classes

data Action =
  GetOAuthToken ByteString

type SlackClientMonad m = (HasHttp m, HasSlackSettings m, MonadThrow m)

slackRequest :: (SlackClientMonad m) => Action -> m (Response LBS.ByteString)
slackRequest action = do
  request <- baseRequest
  clientId <- Text.encodeUtf8 <$> slackClientId
  clientSecret <- slackClientSecret
  makeRequest $ buildRequest request clientId clientSecret action

buildRequest :: Request -> ByteString -> ByteString -> Action -> Request
buildRequest request clientId clientSecret action =
  case action of
    GetOAuthToken code ->
      request
        { method = "GET"
        , path = "/api/oauth.access"
        , queryString =
            "code=" <> code <> "&client_id=" <> clientId <> "&client_secret=" <>
            clientSecret
        }

baseRequest :: (Monad m) => m Request
baseRequest = do
  return $ parseRequest_ "https://slack.com"
