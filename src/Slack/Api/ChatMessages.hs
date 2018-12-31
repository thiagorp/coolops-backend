module Slack.Api.ChatMessages
  ( postMessage
  , updateMessage
  ) where

import Import

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

postMessage :: Text -> Text -> Message -> App (Maybe Text)
postMessage token channel message = do
  response <- slackRequest (PostMessageAsBot params)
  case statusCode (responseStatus response) of
    200 -> return $ fmap responseTs $ decode $ responseBody response
    _ -> return Nothing
  where
    params = ChatPostMessageAsBot token channel message

updateMessage :: Text -> Text -> Text -> Message -> App ()
updateMessage token channel ts message =
  void $ slackRequest (UpdateMessage params)
  where
    params = ChatUpdateMessage token channel ts message
