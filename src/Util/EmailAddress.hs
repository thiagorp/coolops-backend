module Util.EmailAddress
  ( EmailAddress(..)
  , emailAddress
  ) where

import RIO

import Database.PostgreSQL.Simple.FromField
  ( FromField(..)
  , ResultError(ConversionFailed)
  , returnError
  )
import Database.PostgreSQL.Simple.ToField
import qualified Text.Email.Validate as TEV (EmailAddress, emailAddress, toByteString)

newtype EmailAddress =
  EmailAddress TEV.EmailAddress

instance ToField EmailAddress where
  toField (EmailAddress email) = toField (TEV.toByteString email)

instance FromField EmailAddress where
  fromField f bs = (fromField f bs) >>= buildEmail
    where
      buildEmail email =
        case (emailAddress email) of
          Nothing -> returnError ConversionFailed f "invlaid email address"
          Just e -> return e

emailAddress :: ByteString -> Maybe EmailAddress
emailAddress e = EmailAddress <$> TEV.emailAddress e
