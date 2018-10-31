module Auth.UseCases.Login
  ( module Model
  , Params(..)
  , login
  ) where

import RIO

import Auth.Database
import Auth.Domain
import Model

data Params = Params
  { paramEmail :: !EmailAddress
  , paramPassword :: !Text
  }

verify :: Text -> User -> Maybe User
verify password user =
  if authenticate user password
    then Just user
    else Nothing

login :: (HasDb m) => Params -> m (Maybe User)
login Params {..} = do
  maybeUser <- runDb $ findUserByEmail paramEmail
  case maybeUser of
    Nothing -> return Nothing
    Just (Entity _ user) -> return $ verify paramPassword user
