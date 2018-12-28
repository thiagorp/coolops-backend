{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Api.Handlers.Slack.Events where

import Api.Import
import Data.Aeson.Types (Parser)


data Event
  = Challenge Text
  | Unknown

instance FromJSON Event where
  parseJSON = withObject "" $ \o -> do
    eventType <- o .: "type" :: Parser Text
    case eventType of
      "url_verification" -> parseChallenge o
      "event_callback" -> parseEventCallback o
      _ -> return Unknown


parseChallenge :: Object -> Parser Event
parseChallenge o = do
  challenge <- o .: "challenge"
  return $ Challenge challenge


parseEventCallback :: Object -> Parser Event
parseEventCallback _ = return Unknown


ok :: Handler a
ok = sendResponseStatus ok200 ()


handleEvent :: Event -> Handler Value
handleEvent event =
  case event of
    Challenge challenge ->
      return $ String challenge

    Unknown ->
      ok


postSlackEventsR :: Handler Value
postSlackEventsR = requireJsonBody >>= handleEvent
