{-# LANGUAGE OverloadedStrings #-}

module Util.EmailAddress
  ( EmailAddress(..)
  , emailAddressToText
  ) where

import RIO
import qualified RIO.Text as Text

import Data.Aeson
import Database.Persist.Sql
import qualified Text.Email.Validate as TEV (EmailAddress, emailAddress, toByteString, validate)

emailAddressToText :: EmailAddress -> Text
emailAddressToText (EmailAddress email) = decodeUtf8Lenient $ TEV.toByteString email

newtype EmailAddress =
  EmailAddress TEV.EmailAddress
  deriving (Show)

instance PersistField EmailAddress where
  toPersistValue (EmailAddress email) = PersistText $ decodeUtf8Lenient $ TEV.toByteString email
  fromPersistValue (PersistText v) =
    case TEV.validate (encodeUtf8 v) of
      Left err -> Left $ Text.pack err
      Right email -> Right (EmailAddress email)
  fromPersistValue (PersistByteString v) =
    case TEV.validate v of
      Left err -> Left $ Text.pack err
      Right email -> Right (EmailAddress email)
  fromPersistValue _ = Left "Email address needs to be either a text or a bytestring"

instance PersistFieldSql EmailAddress where
  sqlType _ = SqlString

instance FromJSON EmailAddress where
  parseJSON =
    withText "EmailAddress" $ \t ->
      case TEV.emailAddress $ encodeUtf8 t of
        Nothing -> fail "Failed to parse email address"
        Just email -> return (EmailAddress email)
