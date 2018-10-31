module Util.Password
  ( SafePassword
  , RawPassword
  , passwordMatch
  , protectPassword
  ) where

import RIO

import Crypto.PasswordStore (makePassword, verifyPassword)
import Database.Persist.Sql

import Util.Validation

type RawPassword = Validated (SizeGreaterThan 8) ByteString

newtype SafePassword =
  SafePassword ByteString
  deriving (Show)

passwordMatch :: SafePassword -> ByteString -> Bool
passwordMatch (SafePassword safe) raw = verifyPassword raw safe

protectPassword :: (MonadIO m) => RawPassword -> m SafePassword
protectPassword p = liftIO $ SafePassword <$> makePassword (getValue p) 17

instance PersistField SafePassword where
  toPersistValue (SafePassword p) = PersistText $ decodeUtf8Lenient p
  fromPersistValue (PersistText v) = Right $ SafePassword (encodeUtf8 v)
  fromPersistValue (PersistByteString v) = Right $ SafePassword v
  fromPersistValue _ = Left "Password needs to be either a text or a bytestring"

instance PersistFieldSql SafePassword where
  sqlType _ = SqlString
