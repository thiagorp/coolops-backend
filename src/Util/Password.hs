{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Util.Password
  ( SafePassword
  , RawPassword
  , passwordMatch
  , protectPassword
  ) where

import RIO

import Data.Aeson (FromJSON)
import Crypto.PasswordStore (makePassword, verifyPassword)
import Database.Persist.Sql

import Util.Validation

newtype RawPassword =
  RawPassword (Validated (SizeGreaterThan 8) ByteString)
  deriving (FromJSON)

newtype SafePassword =
  SafePassword ByteString

instance Show SafePassword where
  show _ = "[Password]"

passwordMatch :: SafePassword -> ByteString -> Bool
passwordMatch (SafePassword safe) raw = verifyPassword raw safe

protectPassword :: (MonadIO m) => RawPassword -> m SafePassword
protectPassword (RawPassword p) = liftIO $ SafePassword <$> makePassword (getValue p) 17

instance PersistField SafePassword where
  toPersistValue (SafePassword p) = PersistText $ decodeUtf8Lenient p
  fromPersistValue (PersistText v) = Right $ SafePassword (encodeUtf8 v)
  fromPersistValue (PersistByteString v) = Right $ SafePassword v
  fromPersistValue _ = Left "Password needs to be either a text or a bytestring"

instance PersistFieldSql SafePassword where
  sqlType _ = SqlString
