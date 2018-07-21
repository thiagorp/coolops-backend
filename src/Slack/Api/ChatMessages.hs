module Slack.Api.ChatMessages
  ( SlackClientMonad
  , postMessage
  , updateMessage
  ) where

import RIO

import Data.Aeson
import Network.HTTP.Client hiding (Response)
import Network.HTTP.Types

import Slack.Api.ClientBase
import Slack.Api.Message

newtype Response = Response
  { responseTs :: Text
  }

instance FromJSON Response where
  parseJSON =
    withObject "" $ \o -> do
      responseTs <- o .: "ts"
      return Response {..}

postMessage :: SlackClientMonad m => Text -> Text -> Message -> m (Maybe Text)
postMessage token channel message = do
  response <- slackRequest (PostMessageAsBot params)
  case statusCode (responseStatus response) of
    200 -> return $ fmap responseTs $ decode $ responseBody response
    _ -> return Nothing
  where
    params = ChatPostMessageAsBot token channel message

updateMessage :: SlackClientMonad m => Text -> Text -> Text -> Message -> m ()
updateMessage token channel ts message =
  void $ slackRequest (UpdateMessage params)
  where
    params = ChatUpdateMessage token channel ts message
