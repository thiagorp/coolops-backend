{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Api.Handlers.SlackMessages
  ( postSlackMessagesR
  ) where

import Api.Import

import Data.Aeson.Types (Parser)

import qualified Api.Handlers.SlackMessageButtons.DeployBuild as DeployBuild
import qualified Api.Handlers.SlackMessageButtons.ReleaseEnvironmentLock as ReleaseEnvironmentLock
import Slack.Api.Message
import Slack.MessageButtons

data Request = Request
  { reqMessageType :: !MessageButtonAction
  , reqActionValue :: !Text
  , reqTeamId :: !Text
  , reqVerificationToken :: !Text
  , reqSenderId :: !Text
  , reqSenderName :: !Text
  }
  deriving (Show)

actionNameParser :: [Object] -> Parser MessageButtonAction
actionNameParser [] = fail "No action provided"
actionNameParser [n] = n .: "name"
actionNameParser _ = fail "Only one action is supported"

instance FromJSON Request where
  parseJSON =
    withObject "" $ \o -> do
      actions <- o .: "actions"
      teamO <- o .: "team"
      userO <- o .: "user"
      reqTeamId <- teamO .: "id"
      reqSenderId <- userO .: "id"
      reqSenderName <- userO .: "name"
      reqVerificationToken <- o .: "token"
      actionValues <- traverse (.: "value") actions
      reqActionValue <-
        case actionValues of
          [] -> fail "No action provided"
          [e] -> return e
          _ -> fail "Only one action is supported"
      reqMessageType <- actionNameParser actions <|> o .: "callback_id"
      return Request {..}

verifyToken :: Text -> Handler ()
verifyToken token = do
  settings <- slackSettings <$> getYesod
  let configToken = slackVerificationToken settings
  unless (token == configToken) notAuthenticated

handleMessage :: Request -> Handler (Maybe Message)
handleMessage Request {..} =
  case reqMessageType of
    DeployBuild bId eId ->
      DeployBuild.call eId bId reqTeamId (reqSenderId, reqSenderName)

    OldDeployBuildFromCallbackId buildId ->
      case textToUUID reqActionValue of
        Nothing ->
          sendResponseStatus status400 ("Invalid requests" :: Text)

        Just environmentId ->
          DeployBuild.call (EnvironmentKey environmentId) buildId reqTeamId (reqSenderId, reqSenderName)
    
    ReleaseEnvironmentLock lockId ->
      ReleaseEnvironmentLock.call reqSenderId lockId

process :: Request -> Handler Value
process req@Request {..} = do
  verifyToken reqVerificationToken
  response <- handleMessage req
  case response of
    Nothing ->
      sendResponse ()

    Just message ->
      return $ toJSON message

postSlackMessagesR :: Handler Value
postSlackMessagesR = do
  maybeRequest <- lookupPostParam "payload"
  case encodeUtf8 <$> maybeRequest >>= decodeStrict of
    Nothing -> sendResponseStatus status400 ("Invalid request" :: Text)
    Just request -> process request
