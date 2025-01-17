{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Util.UUID
  ( module ExposedUUID
  , genUUID
  , textToUUID
  , uuidToText
  ) where

import RIO
import qualified RIO.Text as Text

import Data.ByteString.Char8 as B8
import Data.UUID as UUID
import Data.UUID as ExposedUUID (UUID)
import Data.UUID.V4 as UUID (nextRandom)
import Database.Persist.Sql
import Yesod.Core.Dispatch

genUUID :: (MonadIO m) => m UUID
genUUID = liftIO UUID.nextRandom

textToUUID :: Text -> Maybe UUID
textToUUID = UUID.fromString . Text.unpack

uuidToText :: UUID -> Text
uuidToText = UUID.toText

instance PersistField UUID where
  toPersistValue = PersistDbSpecific . B8.pack . UUID.toString
  fromPersistValue (PersistDbSpecific t) =
    case UUID.fromString $ B8.unpack t of
      Just x -> Right x
      Nothing -> Left "Invalid UUID"
  fromPersistValue _ = Left "Not PersistDBSpecific"

instance PersistFieldSql UUID where
  sqlType _ = SqlOther "uuid"

instance PathPiece UUID where
  fromPathPiece = UUID.fromText
  toPathPiece = UUID.toText
