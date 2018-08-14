module Util.Key
  ( Key
  , genID
  , keyText
  , keyByteString
  ) where

import RIO

import Data.Aeson (FromJSON(..), ToJSON(..))
import Data.UUID (UUID, toASCIIBytes, toText)
import Data.UUID.V4 (nextRandom)
import Database.PostgreSQL.Simple.FromField (FromField(..))
import Database.PostgreSQL.Simple.ToField

newtype Key a =
  Key UUID
  deriving (Eq, ToField, FromField, ToJSON, FromJSON)

genID :: (MonadIO m) => m (Key a)
genID = liftIO $ Key <$> nextRandom

keyText :: Key a -> Text
keyText (Key uuid) = toText uuid

keyByteString :: Key a -> ByteString
keyByteString (Key uuid) = toASCIIBytes uuid
