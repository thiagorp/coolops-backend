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

instance ToField (Key a) where
  toField (Key uuid) = toField uuid

instance FromField (Key a) where
  fromField f bs = Key <$> (fromField f bs)

instance FromJSON (Key a) where
  parseJSON value = Key <$> (parseJSON value)

instance ToJSON (Key a) where
  toJSON (Key uuid) = toJSON uuid

instance Eq (Key a) where
  (==) (Key a) (Key b) = a == b

genID :: (MonadIO m) => m (Key a)
genID = liftIO $ Key <$> nextRandom

keyText :: Key a -> Text
keyText (Key uuid) = toText uuid

keyByteString :: Key a -> ByteString
keyByteString (Key uuid) = toASCIIBytes uuid
