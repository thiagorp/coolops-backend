module Auth.UseCases.Login
  ( Params(..)
  , login
  ) where

import RIO

import Auth.Classes (UserRepo, findUserByEmail)
import Auth.Domain (RawPassword, User, UserEmail, authenticate)

data Params = Params
  { paramEmail :: !UserEmail
  , paramPassword :: !RawPassword
  }

verify :: RawPassword -> User -> Maybe User
verify password user =
  if authenticate user password
    then Just user
    else Nothing

login :: (UserRepo m, Monad m) => Params -> m (Maybe User)
login Params {..} = do
  maybeUser <- findUserByEmail paramEmail
  return $ maybeUser >>= verify paramPassword
