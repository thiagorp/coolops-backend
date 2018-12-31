module Auth.UseCases.Login
  ( Params(..)
  , login
  ) where

import Import

import Auth.Database
import Auth.Domain

data Params = Params
  { paramEmail :: !EmailAddress
  , paramPassword :: !Text
  }

verify :: Text -> User -> Maybe User
verify password user =
  if authenticate user password
    then Just user
    else Nothing

login :: Params -> App (Maybe User)
login Params {..} = do
  maybeUser <- findUserByEmail paramEmail
  case maybeUser of
    Nothing -> return Nothing
    Just (Entity _ user) -> return $ verify paramPassword user
