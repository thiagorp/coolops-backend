{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Util.Key
  ( Key
  , genID
  , keyByteString
  , keyText
  , keyUUID
  ) where

import RIO hiding (fromString)
import qualified RIO.Text as T

import Data.Aeson (FromJSON(..), ToJSON(..))
import Data.UUID (UUID, fromString, toASCIIBytes, toText)
import Data.UUID.V4 (nextRandom)
import qualified Database.Persist as P
import qualified Database.Persist.Sql as P
import Database.PostgreSQL.Simple.FromField (FromField(..))
import Database.PostgreSQL.Simple.ToField

newtype Key a =
  Key UUID
  deriving (Eq, ToField, FromField, ToJSON, FromJSON, Hashable, Show)

instance P.PersistField (Key a) where
  toPersistValue = P.PersistText . keyText
  fromPersistValue (P.PersistText t) =
    case fromString (T.unpack t) of
      Nothing -> Left "Invalid UUID"
      Just uuid -> Right (Key uuid)
  fromPersistValue _ = Left "UUID type must be a text"

instance P.PersistFieldSql (Key a) where
  sqlType _ = P.SqlString

genID :: (MonadIO m) => m (Key a)
genID = liftIO $ Key <$> nextRandom

keyUUID :: Key a -> UUID
keyUUID (Key uuid) = uuid

keyText :: Key a -> Text
keyText (Key uuid) = toText uuid

keyByteString :: Key a -> ByteString
keyByteString (Key uuid) = toASCIIBytes uuid
