{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Api.Handlers.Slack.Common
  ( verifySignature
  ) where

import Api.Import

import qualified RIO.ByteString.Lazy as LBS
import qualified RIO.List as List
import qualified RIO.Text as T
import qualified Data.Char as Char
import Data.Digest.Pure.SHA
import Text.Printf (printf)

buildRequestDigest :: Handler Text
buildRequestDigest = do
  settings <- slackSettings <$> getYesod
  let secret = LBS.fromStrict (slackSigningSecret settings)
  timestamp <- foldr (<>) "" <$> lookupHeaders "X-Slack-Request-Timestamp"
  body <- rebuildBody <$> getPostParams
  return $ T.pack $ showDigest $ hmacSha256 secret (LBS.fromStrict ("v0:" <> timestamp <> ":" <> body))
  where
    rebuildBody params = List.foldr (<>) "" $ List.intersperse "&" $ List.map glueParams params
    glueParams (k, v) = encodeParam k <> "=" <> encodeParam v
    encodeParam = encodeUtf8 . T.concatMap encodeUriComponent
    encodeUriComponent c
      | c == ' ' = "+"
      | Char.isAlphaNum c = T.singleton c
      | T.any (== c) "-._~" = T.singleton c
      | otherwise = T.pack $ printf "%%%02X" c
      

verifySignature :: Handler ()
verifySignature = do
  requestDigest <- buildRequestDigest
  headerDigest <- decodeUtf8Lenient . foldr (<>) "" <$> lookupHeaders "X-Slack-Signature"
  unless ("v0=" <> requestDigest == headerDigest) (permissionDenied "")
