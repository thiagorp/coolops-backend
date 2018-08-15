module Handlers.Messages
  ( call
  ) where

import RIO
import qualified RIO.Text.Lazy as LText

import Data.Aeson hiding (json)
import Network.HTTP.Types.Status (status400, status401)
import Web.Scotty.Trans

import qualified Handlers.MessageButtons.DeployBuild as DeployBuild
import Slack.Api.Classes
import Slack.MessageButtons
import Types

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

verifyToken :: Text -> WebHandler ()
verifyToken token = do
  configToken <- lift slackVerificationToken
  if token == configToken
    then return ()
    else status status401 >> finish

handleMessage :: Request -> WebHandler ()
handleMessage Request {..} =
  case reqMessageType of
    DeployBuild buildId ->
      DeployBuild.call
        reqActionValue
        buildId
        reqTeamId
        (reqSenderId, reqSenderName)

process :: Request -> WebHandler ()
process req@Request {..} = do
  verifyToken reqVerificationToken
  handleMessage req

call :: WebHandler ()
call = do
  eitherReq <- eitherDecode <$> param "payload"
  case eitherReq of
    Right req -> process req
    Left err -> text (LText.pack err) >> status status400
