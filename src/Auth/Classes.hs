module Auth.Classes where

import RIO

import Auth.Domain

class UserRepo m where
  createUser :: User -> m ()
  findUserByEmail :: UserEmail -> m (Maybe User)
  findUserByAccessToken :: Text -> m (Maybe User)

class CompanyRepo m where
  createCompany :: Company -> m ()
  listCompanies :: m [Company]
