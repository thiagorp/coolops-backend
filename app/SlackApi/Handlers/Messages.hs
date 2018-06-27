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
import Slack.Classes
import Slack.Domain.Team (Team)
import Slack.MessageButtons
import Types

data Request = Request
  { reqMessageType :: !MessageButtonAction
  , reqActionValue :: !Text
  , reqTeamId :: !Text
  , reqVerificationToken :: !Text
  , reqSender :: !Text
  }

instance FromJSON Request where
  parseJSON =
    withObject "" $ \o -> do
      reqMessageType <- o .: "callback_id"
      actions <- o .: "actions"
      teamO <- o .: "team"
      userO <- o .: "user"
      reqTeamId <- teamO .: "id"
      reqSender <- userO .: "id"
      reqVerificationToken <- o .: "token"
      actionValues <- mapM (\action -> action .: "value") actions
      reqActionValue <-
        case actionValues of
          [] -> fail "No action provided"
          e:[] -> return e
          _:_ -> fail "Only one action is supported"
      return Request {..}

verifyToken :: Text -> WebHandler ()
verifyToken token = do
  configToken <- lift slackVerificationToken
  case token == configToken of
    True -> return ()
    False -> status status401 >> finish

handleMessage :: Team -> Request -> WebHandler ()
handleMessage slackTeam Request {..} =
  case reqMessageType of
    DeployBuild buildId ->
      DeployBuild.call slackTeam reqActionValue buildId reqSender

process :: Request -> WebHandler ()
process req@(Request {..}) = do
  verifyToken reqVerificationToken
  maybeSlackTeam <- lift $ getSlackTeam reqTeamId
  case maybeSlackTeam of
    Nothing -> status status401 >> finish
    Just slackTeam -> handleMessage slackTeam req

call :: WebHandler ()
call = do
  eitherReq <- eitherDecode <$> param "payload"
  case eitherReq of
    Right req -> process req
    Left err -> text (LText.pack err) >> status status400
