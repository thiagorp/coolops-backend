module Slack.MessageButtons where

import Import
import qualified RIO.Text as Text

import Data.Aeson
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P

data MessageButtonAction
  = DeployBuild Text
  | ReleaseEnvironmentLock EnvironmentLockId

instance FromJSON MessageButtonAction where
  parseJSON =
    withText "message_action" $ \t ->
      case parseAction t of
        Just m -> return m
        Nothing -> fail "Wrong message type"

parseAction :: Text -> Maybe MessageButtonAction
parseAction =
  P.parseMaybe $
    deployBuildParser
    <|> releaseEnvironmentLockParser

deployBuildParser :: P.Parsec Void Text MessageButtonAction
deployBuildParser = do
  _ <- P.string "deploy_build|"
  buildId <- Text.pack <$> P.many (P.alphaNumChar P.<|> P.char '-')
  P.eof
  return $ DeployBuild buildId

releaseEnvironmentLockParser :: P.Parsec Void Text MessageButtonAction
releaseEnvironmentLockParser = do
  _ <- P.string "release_lock|"
  lockId <- uuidParser
  P.eof
  return $ ReleaseEnvironmentLock (EnvironmentLockKey lockId)

uuidParser :: P.Parsec Void Text UUID
uuidParser = do
  s <- P.many (P.alphaNumChar <|> P.char '-')
  case textToUUID (Text.pack s) of
    Nothing -> fail "invalid uuid"
    Just u -> return u
