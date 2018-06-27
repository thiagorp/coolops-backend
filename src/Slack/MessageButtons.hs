module Slack.MessageButtons where

import RIO
import qualified RIO.Text as Text

import Data.Aeson
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P

data MessageButtonAction =
  DeployBuild Text

instance FromJSON MessageButtonAction where
  parseJSON =
    withText "message_action" $ \t ->
      case parseAction t of
        Right m -> return m
        Left _ -> fail "Wrong message type"

parseAction ::
     Text -> Either (P.ParseError (P.Token Text) Void) MessageButtonAction
parseAction = P.parse actionParser ""

actionParser :: P.Parsec Void Text MessageButtonAction
actionParser = do
  _ <- P.string "deploy_build|"
  buildId <- Text.pack <$> P.many (P.alphaNumChar P.<|> P.char '-')
  return $ DeployBuild buildId
