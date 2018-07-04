module Database.Queries.Profile where

import RIO

import Data.UUID
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow

import Auth.Domain (UserID)
import Common.Database

data Profile = Profile
  { profileId :: !UUID
  , profileUserFirstName :: !Text
  , profileUserLastName :: !Text
  , profileUserEmail :: !Text
  , profileCompanyId :: !UUID
  , profileCompanyName :: !Text
  }

instance FromRow Profile where
  fromRow = do
    profileId <- field
    profileUserFirstName <- field
    profileUserLastName <- field
    profileUserEmail <- field
    profileCompanyId <- field
    profileCompanyName <- field
    return Profile {..}

getProfile :: HasPostgres m => UserID -> m (Maybe Profile)
getProfile userId = do
  profiles <- runQuery q (Only userId)
  case profiles of
    [] -> return Nothing
    profile:_ -> return $ Just profile
  where
    q =
      "select u.id, u.first_name, u.last_name, u.email, c.id, c.name\
        \ from users u\
        \ left join companies c on c.id = u.company_id\
        \ where u.id = ?"
