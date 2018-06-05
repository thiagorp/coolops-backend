module Util.Key
  ( Key
  , genID
  , keyText
  ) where

import RIO

import Data.UUID (UUID, toText)
import Data.UUID.V4 (nextRandom)
import Database.PostgreSQL.Simple.FromField (FromField(..))
import Database.PostgreSQL.Simple.ToField

newtype Key a =
  Key UUID

instance ToField (Key a) where
  toField (Key uuid) = toField uuid

instance FromField (Key a) where
  fromField f bs = Key <$> (fromField f bs)

genID :: (MonadIO m) => m (Key a)
genID = liftIO $ Key <$> nextRandom

keyText :: Key a -> Text
keyText (Key uuid) = toText uuid
