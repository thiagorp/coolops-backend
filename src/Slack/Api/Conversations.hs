module Slack.Api.Conversations
  ( Conversation(..)
  , Response(..)
  , SlackClientMonad
  , list
  ) where

import RIO

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

newtype Response = Response
  { conversations :: [Conversation]
  }

instance FromJSON Response where
  parseJSON =
    withObject "" $ \o -> do
      conversations <- o .: "channels"
      return Response {..}

list :: (SlackClientMonad m) => Text -> m Response
list token = do
  response <- slackRequest (ListConversations token)
  case statusCode (responseStatus response) of
    200 -> return $ fromMaybe (Response {conversations = []}) $decode (responseBody response)
    _ -> return Response {conversations = []}
