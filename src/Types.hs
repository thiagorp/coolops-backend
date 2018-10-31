{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeOperators #-}

module Types
  ( module Types
  , module Util.Validation
  ) where

import RIO
import qualified RIO.ByteString.Lazy as LazyByteString

import Data.Aeson
import Data.ByteString.Base58 (encodeBase58I, rippleAlphabet)
import Database.Persist.Sql
import System.Random (randomRIO)
import Util.Validation

type BuildName = Validated (SizeGreaterThan 1) Text

type CompanyName = Validated (SizeGreaterThan 2) Text

type EnvironmentName = Validated (SizeGreaterThan 1) Text

type ProjectName = Validated (SizeGreaterThan 1) Text

type DockerImage = Validated (SizeGreaterThan 1) Text

type Slug = Validated (IsSlug && SizeGreaterThan 1) Text

type UserName = Validated (SizeGreaterThan 2) Text

newtype AccessToken =
  AccessToken Text
  deriving (PersistField, PersistFieldSql, Show)

genAccessToken :: (MonadIO m) => m AccessToken
genAccessToken = genAccessToken' 36 ""

genAccessToken' :: (MonadIO m) => Integer -> ByteString -> m AccessToken
genAccessToken' size token = do
  i <- liftIO $ randomRIO (0, 57)
  let randomChar = encodeBase58I rippleAlphabet i
  case size of
    0 -> return $ AccessToken (decodeUtf8Lenient token)
    _ -> genAccessToken' (size - 1) (token <> randomChar)

instance ToJSON AccessToken where
  toJSON (AccessToken token) = toJSON token

data DeploymentStatus
  = Queued
  | Running
  | Succeeded
  | Failed Text
  deriving (Eq, Show)

instance PersistField DeploymentStatus where
  toPersistValue = PersistText . statusText
  fromPersistValue (PersistText t) = Right (textToStatus t)
  fromPersistValue _ = Left "Database status must be a Text"

instance PersistFieldSql DeploymentStatus where
  sqlType _ = SqlString

textToStatus :: Text -> DeploymentStatus
textToStatus text =
  case text of
    "queued" -> Queued
    "running" -> Running
    "finished_with_success" -> Succeeded
    "failed_with_invalid_docker_image" -> Failed "invalid_docker_image"
    "failed_with_job_failed" -> Failed "job_failed"
    "failed_with_job_not_found" -> Failed "job_not_found"
    _ -> Failed "unknown_reason"

statusText :: DeploymentStatus -> Text
statusText status =
  case status of
    Queued -> "queued"
    Running -> "running"
    Succeeded -> "finished_with_success"
    Failed reason -> "failed_with_" <> reason

isFinished :: DeploymentStatus -> Bool
isFinished status =
  case status of
    Queued -> False
    Running -> False
    Succeeded -> True
    Failed _ -> True

instance PersistField (HashMap Text Text) where
  toPersistValue = PersistDbSpecific . LazyByteString.toStrict . encode
  fromPersistValue (PersistText v) =
    case eitherDecodeStrict (encodeUtf8 v) of
      Left _ -> Left "Invalid HashMap"
      Right h -> Right h
  fromPersistValue (PersistByteString v) =
    case eitherDecodeStrict v of
      Left _ -> Left "Invalid HashMap"
      Right h -> Right h
  fromPersistValue _ = Left "HashMap should come as string or bytea"

instance PersistFieldSql (HashMap Text Text) where
  sqlType _ = SqlOther "JSONB"
