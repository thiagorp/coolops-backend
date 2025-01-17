{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Slack.Api.ClientBase
  ( Action(..)
  , ChatPostEphemeralMessage(..)
  , ChatPostMessageAsBot(..)
  , ChatUpdateMessage(..)
  , SlackClientMonad
  , SlackResponse
  , slackRequest
  ) where

import Import

import Data.Aeson
import qualified RIO.ByteString.Lazy as LBS
import qualified RIO.Text as Text

import Network.HTTP.Client
import Network.HTTP.Types

import Http.Classes
import Slack.Api.Message (Message(..))

type SlackResponse = Response LBS.ByteString

data ChatPostMessageAsBot
  = ChatPostMessageAsBot Text Text Message 

instance ToJSON ChatPostMessageAsBot where
  toJSON (ChatPostMessageAsBot _ channel Message {..}) =
    object
      [ "channel" .= channel
      , "text" .= messageText
      , "attachments" .= messageAttachments
      , "as_user" .= True
      ]

data ChatUpdateMessage
  = ChatUpdateMessage Text Text Text Message

instance ToJSON ChatUpdateMessage where
  toJSON (ChatUpdateMessage _ channel ts Message {..}) =
    object
      [ "channel" .= channel
      , "text" .= messageText
      , "ts" .= ts
      , "attachments" .= messageAttachments
      ]

data ChatPostEphemeralMessage
  = ChatPostEphemeralMessage Text Text Message

instance ToJSON ChatPostEphemeralMessage where
  toJSON (ChatPostEphemeralMessage channel userId Message {..}) =
    object
      [ "channel" .= channel
      , "text" .= messageText
      , "user" .= userId
      , "attachments" .= messageAttachments
      , "as_user" .= True
      ]

data Action
  = GetOAuthToken ByteString
  | IncomingWebhook Text Message
  | ListConversations Text (Maybe Text)
  | PostEphemeral Text ChatPostEphemeralMessage
  | PostMessageAsBot ChatPostMessageAsBot
  | RevokeToken ByteString
  | UpdateMessage ChatUpdateMessage

type SlackClientMonad m = (HasEnv m, MonadIO m, MonadThrow m)

slackRequest :: Action -> App (Response LBS.ByteString)
slackRequest action = do
  settings <- slackSettings <$> getEnv
  request <- baseRequest
  let clientId = Text.encodeUtf8 (slackClientId settings)
  let clientSecret = slackClientSecret settings
  makeRequest $ buildRequest request clientId clientSecret action

buildRequest :: Request -> ByteString -> ByteString -> Action -> Request
buildRequest request clientId clientSecret action =
  case action of
    GetOAuthToken code ->
      request
        { method = "GET"
        , path = "/api/oauth.access"
        , queryString = "code=" <> code <> "&client_id=" <> clientId <> "&client_secret=" <> clientSecret
        }

    IncomingWebhook url message ->
      (parseRequest_ $ Text.unpack url)
        { method = "POST"
        , requestBody = RequestBodyLBS $ encode message
        }

    ListConversations token cursor ->
      request
        { method = "GET"
        , path = "/api/conversations.list"
        , queryString =
            "token=" <> encodeUtf8 token <> "&exclude_archived=true&limit=1000&types=public_channel,private_channel" <>
            maybe "" (("&cursor=" <>) . encodeUtf8) cursor
        }

    PostMessageAsBot message@(ChatPostMessageAsBot token _ _) ->
      request
        { method = "POST"
        , path = "/api/chat.postMessage"
        , requestBody = RequestBodyLBS $ encode message
        , requestHeaders = [ (hContentType, "application/json"), (hAuthorization, "Bearer " <> encodeUtf8 token) ]
        }

    PostEphemeral token message ->
      request
        { method = "POST"
        , path = "/api/chat.postEphemeral"
        , requestBody = RequestBodyLBS $ encode message
        , requestHeaders = [ (hContentType, "application/json"), (hAuthorization, "Bearer " <> encodeUtf8 token) ]
        }

    RevokeToken token ->
      request
        { method = "GET"
        , path = "/api/auth.revoke"
        , queryString = "token=" <> token
        }

    UpdateMessage message@(ChatUpdateMessage token _ _ _) ->
      request
        { method = "POST"
        , path = "/api/chat.update"
        , requestBody = RequestBodyLBS $ encode message
        , requestHeaders = [(hContentType, "application/json"), (hAuthorization, "Bearer " <> encodeUtf8 token)]
        }

baseRequest :: (Monad m) => m Request
baseRequest = return $ parseRequest_ "https://slack.com"
