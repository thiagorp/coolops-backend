module Auth.Database where

import RIO

import Data.Pool
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Migration

import Auth.Domain

class HasPostgresConnection m where
  getPostgresConn :: m Connection

class (Monad m, HasPostgresConnection m, MonadIO m) =>
      HasPostgres m
  where
  runDb :: (ToRow a) => Query -> a -> m Int64
  runDb q a = do
    conn <- getPostgresConn
    liftIO $ execute conn q a
  runDb' :: (ToRow a) => Query -> a -> m ()
  runDb' q a = void (runDb q a)
  runQuery :: (ToRow a, FromRow b) => Query -> a -> m [b]
  runQuery q a = do
    conn <- getPostgresConn
    liftIO $ query conn q a

migrateDb :: Pool Connection -> IO ()
migrateDb pool =
  withResource pool $ \conn ->
    void $ withTransaction conn (runMigration (ctx conn))
  where
    ctx = MigrationContext cmd False
    cmd =
      MigrationCommands
        [MigrationInitialization, MigrationDirectory "migrations"]

runTransaction :: (HasPostgresConnection m, MonadIO m) => IO a -> m a
runTransaction tx = do
  conn <- getPostgresConn
  liftIO $ withTransaction conn tx

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

findCompanyByAccessToken :: (HasPostgres m) => Text -> m (Maybe Company)
findCompanyByAccessToken token = do
  result <- runQuery q (Only token)
  case result of
    [(key, name, accessToken)] -> return $ Just $ Company key name accessToken
    _ -> return Nothing
  where
    q =
      "select id, name, access_token from companies\
      \ where access_token = ? limit 1"
