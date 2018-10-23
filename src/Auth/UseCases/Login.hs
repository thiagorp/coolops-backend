module Auth.UseCases.Login
  ( Params(..)
  , login
  ) where

import RIO

import Auth.Database
import Auth.Domain (User, UserEmail, authenticate)
import Common.Database

data Params = Params
  { paramEmail :: !UserEmail
  , paramPassword :: !Text
  }

verify :: Text -> User -> Maybe User
verify password user =
  if authenticate user password
    then Just user
    else Nothing

login :: (HasPostgres m) => Params -> m (Maybe User)
login Params {..} = do
  maybeUser <- findUserByEmail paramEmail
  return $ maybeUser >>= verify paramPassword
