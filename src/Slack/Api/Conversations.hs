{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Slack.Api.Conversations
  ( Conversation(..)
  , Response(..)
  , list
  ) where

import Import

import Data.Aeson
import Network.HTTP.Client hiding (Response)
import Network.HTTP.Types

import Slack.Api.ClientBase

data Conversation = Conversation
  { conversationId :: !Text
  , conversationName :: !Text
  }

instance FromJSON Conversation where
  parseJSON =
    withObject "" $ \o -> do
      conversationId <- o .: "id"
      conversationName <- o .: "name"
      return Conversation {..}

data IntermediateResponse = IntermediateResponse
  { conversations_ :: ![Conversation]
  , nextCursor :: !Text
  }

instance FromJSON IntermediateResponse where
  parseJSON =
    withObject "" $ \o -> do
      conversations_ <- o .: "channels"
      metadataO <- o .: "response_metadata"
      nextCursor <- metadataO .: "next_cursor"
      return IntermediateResponse {..}

newtype Response = Response
  { conversations :: [Conversation]
  }

emptyIntermediateResponse :: IntermediateResponse
emptyIntermediateResponse = IntermediateResponse {conversations_ = [], nextCursor = ""}

fetchRecursive :: Text -> [Conversation] -> Maybe Text -> App [Conversation]
fetchRecursive token conversations cursor = do
  response <- slackRequest (ListConversations token cursor)
  case statusCode (responseStatus response) of
    200 -> do
      let IntermediateResponse {..} = fromMaybe emptyIntermediateResponse $ decode (responseBody response)
      let newConversations = conversations <> conversations_
      case nextCursor of
        "" -> return newConversations
        newCursor -> fetchRecursive token newConversations (Just newCursor)
    _ -> return conversations

list :: Text -> App Response
list token = do
  conversations <- fetchRecursive token [] Nothing
  return Response {..}
