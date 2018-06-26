module Handlers.Messages
  ( call
  ) where

import RIO
import qualified RIO.Text as Text
import qualified RIO.Text.Lazy as LText

import Data.Aeson hiding (json)
import Network.HTTP.Types.Status (status400, status401)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import Web.Scotty.Trans

import Deployments.Classes
import qualified Deployments.Domain.Build as B
import qualified Deployments.Domain.Environment as E
import qualified Deployments.UseCases.CreateDeployment as App
import Slack.Api.Classes
import Slack.Api.IncomingWebhooks
import Slack.Api.Message
import Slack.Classes
import Slack.Domain.Team (Team(..), webhookUrl)
import Types

data MessageType =
  DeployBuild Text

data Request = Request
  { reqMessageType :: !MessageType
  , reqEnvironmentId :: !Text
  , reqTeamId :: !Text
  , reqVerificationToken :: !Text
  , reqSender :: !Text
  }

instance FromJSON Request where
  parseJSON =
    withObject "" $ \o -> do
      message <- o .: "callback_id"
      actions <- o .: "actions"
      teamO <- o .: "team"
      userO <- o .: "user"
      reqTeamId <- teamO .: "id"
      reqSender <- userO .: "id"
      reqVerificationToken <- o .: "token"
      environmentIds <- mapM (\action -> action .: "value") actions
      reqEnvironmentId <-
        case environmentIds of
          [] -> fail "No action provided"
          e:[] -> return e
          _:_ -> fail "Only one action is supported"
      reqMessageType <-
        case parseMessageType message of
          Right m -> return m
          Left _ -> fail "Wrong message type"
      return Request {..}

parseMessageType ::
     Text -> Either (P.ParseError (P.Token Text) Void) MessageType
parseMessageType = P.parse messageTypeParser ""

messageTypeParser :: P.Parsec Void Text MessageType
messageTypeParser = do
  _ <- P.string "deploy_build|"
  buildId <- Text.pack <$> P.many (P.alphaNumChar P.<|> P.char '-')
  return $ DeployBuild buildId

runApp :: B.Build -> E.Environment -> WebHandler ()
runApp build environment =
  lift $ App.call (App.Params build environment) >> return ()

verifyToken :: Text -> WebHandler ()
verifyToken token = do
  configToken <- lift slackVerificationToken
  case token == configToken of
    True -> return ()
    False -> status status401 >> finish

buildMessage :: Text -> B.Build -> E.Environment -> Message
buildMessage senderId B.Build {..} E.Environment {..} =
  slackMessage {messageText = Just t}
  where
    t =
      "<@" <> senderId <> "> deployed *" <> B.nameText buildName <> "* to *" <>
      E.nameText environmentName <>
      "*"

process :: Request -> WebHandler ()
process Request {..} = do
  verifyToken reqVerificationToken
  case reqMessageType of
    DeployBuild buildId -> do
      maybeSlackTeam <- lift $ getSlackTeam reqTeamId
      case maybeSlackTeam of
        Nothing -> status status401 >> finish
        Just Team {..} -> do
          maybeBuild <- lift $ getBuild teamCompanyId buildId
          case maybeBuild of
            Nothing -> text "_This build is gone_"
            Just build -> do
              maybeEnvironment <-
                lift $ getEnvironment teamCompanyId reqEnvironmentId
              case maybeEnvironment of
                Nothing -> text "_This environment doesn't exist anymore_"
                Just environment -> do
                  runApp build environment
                  lift $
                    sendIncomingWebhook
                      (webhookUrl teamIncomingWebhook)
                      (buildMessage reqSender build environment)

call :: WebHandler ()
call = do
  eitherReq <- eitherDecode <$> param "payload"
  case eitherReq of
    Right req -> process req
    Left err -> text (LText.pack err) >> status status400
