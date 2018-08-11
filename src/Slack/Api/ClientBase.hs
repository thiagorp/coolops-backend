module Slack.Api.ClientBase
  ( Action(..)
  , ChatPostMessageAsBot(..)
  , ChatUpdateMessage(..)
  , SlackClientMonad
  , SlackResponse
  , slackRequest
  ) where

import RIO

import Data.Aeson
import qualified RIO.ByteString.Lazy as LBS
import qualified RIO.Text as Text

import Network.HTTP.Client
import Network.HTTP.Types

import Http.Classes
import Slack.Api.Classes
import Slack.Api.Message (Message(..))

type SlackResponse = Response LBS.ByteString

data ChatPostMessageAsBot =
  ChatPostMessageAsBot Text
                       Text
                       Message

instance ToJSON ChatPostMessageAsBot where
  toJSON (ChatPostMessageAsBot _ channel Message {..}) =
    object
      [ "channel" .= channel
      , "text" .= messageText
      , "attachments" .= messageAttachments
      ]

data ChatUpdateMessage =
  ChatUpdateMessage Text
                    Text
                    Text
                    Message

instance ToJSON ChatUpdateMessage where
  toJSON (ChatUpdateMessage _ channel ts Message {..}) =
    object
      [ "channel" .= channel
      , "text" .= messageText
      , "ts" .= ts
      , "attachments" .= messageAttachments
      ]

data Action
  = GetOAuthToken ByteString
  | GetWorkspaceToken ByteString
  | RevokeToken ByteString
  | PostMessageAsBot ChatPostMessageAsBot
  | UpdateMessage ChatUpdateMessage
  | IncomingWebhook Text
                    Message

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
    GetWorkspaceToken code ->
      request
        { method = "GET"
        , path = "/api/oauth.access"
        , queryString =
            "code=" <> code <> "&client_id=" <> clientId <> "&client_secret=" <>
            clientSecret
        }
    RevokeToken token ->
      request
        { method = "GET"
        , path = "/api/auth.revoke"
        , queryString = "token=" <> token
        }
    IncomingWebhook url message ->
      (parseRequest_ $ Text.unpack url)
        {method = "POST", requestBody = RequestBodyLBS $ encode message}
    PostMessageAsBot message@(ChatPostMessageAsBot token _ _) ->
      request
        { method = "POST"
        , path = "/api/chat.postMessage"
        , requestBody = RequestBodyLBS $ encode message
        , requestHeaders =
            [ (hContentType, "application/json")
            , (hAuthorization, "Bearer " <> encodeUtf8 token)
            ]
        }
    UpdateMessage message@(ChatUpdateMessage token _ _ _) ->
      request
        { method = "POST"
        , path = "/api/chat.update"
        , requestBody = RequestBodyLBS $ encode message
        , requestHeaders =
            [ (hContentType, "application/json")
            , (hAuthorization, "Bearer " <> encodeUtf8 token)
            ]
        }

baseRequest :: (Monad m) => m Request
baseRequest = return $ parseRequest_ "https://slack.com"
