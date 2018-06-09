module Auth.Database where

import RIO

import Database.PostgreSQL.Simple

import Auth.Domain
import Common.Database

createUser :: (HasPostgres m) => User -> m ()
createUser User {..} = runDb' q values
  where
    q =
      "insert into users (id, first_name, last_name, email, password, access_token, company_id, created_at, updated_at) values\
        \ (?, ?, ?, ?, ?, ?, ?, NOW(), NOW())"
    values =
      ( userId
      , userFirstName
      , userLastName
      , userEmail
      , userPassword
      , userAccessToken
      , userCompanyId)

findUserByEmail :: (HasPostgres m) => UserEmail -> m (Maybe User)
findUserByEmail search = do
  result <- runQuery q (Only search)
  case result of
    [(key, firstName, lastName, email, password, accessToken, companyId)] ->
      return $
      Just $ User key firstName lastName email password accessToken companyId
    _ -> return Nothing
  where
    q =
      "select id, first_name, last_name, email, password, access_token, company_id from users\
        \ where email = ? limit 1"

createCompany :: (HasPostgres m) => Company -> m ()
createCompany Company {..} = runDb' q values
  where
    q =
      "insert into companies (id, name, access_token, created_at, updated_at) values\
        \ (?, ?, ?, NOW(), NOW())"
    values = (companyId, companyName, companyToken)

findUserByAccessToken :: (HasPostgres m) => Text -> m (Maybe User)
findUserByAccessToken token = do
  result <- runQuery q (Only token)
  case result of
    [(key, firstName, lastName, email, password, accessToken, companyId)] ->
      return $
      Just $ User key firstName lastName email password accessToken companyId
    _ -> return Nothing
  where
    q =
      "select id, first_name, last_name, email, password, access_token, company_id from users\
      \ where access_token = ? limit 1"
