module Auth.Domain
  ( authenticate
  ) where

import RIO

import Model
import Util.Password

authenticate :: User -> Text -> Bool
authenticate user password = passwordMatch (userPassword user) (encodeUtf8 password)
