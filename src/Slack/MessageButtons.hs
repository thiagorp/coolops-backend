module Slack.MessageButtons
  ( MessageButtonAction(..)
  , actionToText
  ) where

import Import
import qualified RIO.Text as Text

import Data.Aeson
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P



data MessageButtonAction
  = OldDeployBuildFromCallbackId BuildId
  | DeployBuild BuildId EnvironmentId
  | ReleaseEnvironmentLock EnvironmentLockId
  deriving (Show)


instance FromJSON MessageButtonAction where
  parseJSON =
    withText "message_action" $ \t ->
      case P.parseMaybe parseAction t of
        Just m -> return m
        Nothing -> fail "Wrong message type"


actionToText :: MessageButtonAction -> Text
actionToText action =
  case action of
    DeployBuild (BuildKey bId) (EnvironmentKey eId) ->
      "deploy_build|" <> uuidToText bId <> "|" <> uuidToText eId

    OldDeployBuildFromCallbackId (BuildKey bId) ->
      "deploy_build|" <> uuidToText bId

    ReleaseEnvironmentLock (EnvironmentLockKey lId) ->
      "release_lock|" <> uuidToText lId


parseAction :: P.Parsec Void Text MessageButtonAction
parseAction =
  P.try deployBuildParser
    P.<|> P.try oldDeployBuildParser
    P.<|> releaseEnvironmentLockParser


deployBuildParser :: P.Parsec Void Text MessageButtonAction
deployBuildParser = do
  _ <- P.string "deploy_build|"
  buildId <- BuildKey <$> uuidParser
  _ <- P.char '|'
  environmentId <- EnvironmentKey <$> uuidParser
  P.eof
  return $ DeployBuild buildId environmentId


oldDeployBuildParser :: P.Parsec Void Text MessageButtonAction
oldDeployBuildParser = do
  _ <- P.string "deploy_build|"
  buildId <- BuildKey <$> uuidParser
  P.eof
  return $ OldDeployBuildFromCallbackId buildId


releaseEnvironmentLockParser :: P.Parsec Void Text MessageButtonAction
releaseEnvironmentLockParser = do
  _ <- P.string "release_lock|"
  lockId <- uuidParser
  P.eof
  return $ ReleaseEnvironmentLock (EnvironmentLockKey lockId)


uuidParser :: P.Parsec Void Text UUID
uuidParser = do
  s <- P.many (P.alphaNumChar P.<|> P.char '-')
  case textToUUID (Text.pack s) of
    Nothing -> fail "invalid uuid"
    Just u -> return u
