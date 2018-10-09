module Api.Handlers.SlackCommands
  ( postSlackCommandsR
  ) where

import qualified RIO.ByteString.Lazy as LBS
import qualified RIO.List as List
import qualified RIO.Text as T

import Api.Import

import Data.Digest.Pure.SHA
import Network.HTTP.Types.URI (urlEncode)
import Text.Megaparsec
import Text.Megaparsec.Char

import Slack.Api.Classes

buildRequestDigest :: Handler Text
buildRequestDigest = do
  secret <- LBS.fromStrict <$> slackSigningSecret
  timestamp <- foldr (<>) "" <$> lookupHeaders "X-Slack-Request-Timestamp"
  body <- rebuildBody <$> getPostParams
  return $ T.pack $ showDigest $ hmacSha256 secret (LBS.fromStrict ("v0:" <> timestamp <> ":" <> body))
  where
    rebuildBody params = List.foldr (<>) "" $ List.intersperse "&" $ List.map glueParams params
    glueParams (k, v) = encodeParam k <> "=" <> encodeParam v
    encodeParam = urlEncode True . encodeUtf8

verifySignature :: Handler ()
verifySignature = do
  requestDigest <- buildRequestDigest
  headerDigest <- decodeUtf8Lenient . foldr (<>) "" <$> lookupHeaders "X-Slack-Signature"
  unless ("v0=" <> requestDigest == headerDigest) (permissionDenied "")

data Command =
  Help

commandParser :: Parsec Void Text Command
commandParser = do
  _ <- string' "help"
  return Help

handleCommand :: Handler Value
handleCommand = do
  commandText <- fromMaybe "" . lookup "text" <$> getPostParams
  case parseMaybe commandParser commandText of
    Nothing -> return $ String "Command not found"
    Just command -> return $ object [("command", String "Help")]

postSlackCommandsR :: Handler Value
postSlackCommandsR = do
  verifySignature
  handleCommand
