module Auth.UseCases.Login
  ( Login(..)
  , login
  ) where

import RIO

import Auth.Classes (UserRepo, findUserByEmail)
import Auth.Domain (RawPassword, User, UserEmail, authenticate)

data Login = Login
  { loginEmail :: !UserEmail
  , loginPassword :: !RawPassword
  }

verify :: RawPassword -> User -> Maybe User
verify password user =
  case authenticate user password of
    True -> Just user
    False -> Nothing

login :: (UserRepo m, Monad m) => Login -> m (Maybe User)
login Login {..} = do
  maybeUser <- findUserByEmail loginEmail
  return $ maybeUser >>= verify loginPassword
