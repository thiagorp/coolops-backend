module Auth.Classes where

import RIO

import Auth.Domain

class HasDBTransaction m where
  runTransaction :: m a -> m a

class UserRepo m where
  createUser :: User -> m ()
  findUserByEmail :: UserEmail -> m (Maybe User)

class CompanyRepo m where
  createCompany :: Company -> m ()
  findCompanyByAccessToken :: Text -> m (Maybe Company)
