module Api.Handlers.SlackMessages
  ( postSlackMessagesR
  ) where

import Api.Import

import qualified Api.Handlers.SlackMessageButtons.DeployBuild as DeployBuild
import Slack.MessageButtons

data Request = Request
  { reqMessageType :: !MessageButtonAction
  , reqActionValue :: !Text
  , reqTeamId :: !Text
  , reqVerificationToken :: !Text
  , reqSenderId :: !Text
  , reqSenderName :: !Text
  }

instance FromJSON Request where
  parseJSON =
    withObject "" $ \o -> do
      reqMessageType <- o .: "callback_id"
      actions <- o .: "actions"
      teamO <- o .: "team"
      userO <- o .: "user"
      reqTeamId <- teamO .: "id"
      reqSenderId <- userO .: "id"
      reqSenderName <- userO .: "name"
      reqVerificationToken <- o .: "token"
      actionValues <- mapM (.: "value") actions
      reqActionValue <-
        case actionValues of
          [] -> fail "No action provided"
          [e] -> return e
          _ -> fail "Only one action is supported"
      return Request {..}

verifyToken :: Text -> Handler ()
verifyToken token = do
  settings <- slackSettings <$> getYesod
  let configToken = slackVerificationToken settings
  unless (token == configToken) notAuthenticated

handleMessage :: Request -> Handler ()
handleMessage Request {..} =
  case reqMessageType of
    DeployBuild buildId -> DeployBuild.call reqActionValue buildId reqTeamId (reqSenderId, reqSenderName)

process :: Request -> Handler ()
process req@Request {..} = do
  verifyToken reqVerificationToken
  handleMessage req

postSlackMessagesR :: Handler ()
postSlackMessagesR = do
  maybeRequest <- lookupPostParam "payload"
  case encodeUtf8 <$> maybeRequest >>= decodeStrict of
    Nothing -> sendResponseStatus status400 ("Invalid request" :: Text)
    Just request -> process request
