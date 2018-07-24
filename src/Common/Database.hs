module Common.Database where

import RIO

import qualified Control.Exception as E
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Migration

class HasDBTransaction m where
  runTransaction :: m a -> m a
  runEitherTransaction :: m (Either e a) -> m (Either e a)

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
  runQuery_ :: (FromRow b) => Query -> m [b]
  runQuery_ q = do
    conn <- getPostgresConn
    liftIO $ query_ conn q

migrateDb :: Connection -> IO ()
migrateDb conn = void $ withTransaction conn (runMigration (ctx conn))
  where
    ctx = MigrationContext cmd False
    cmd = MigrationDirectory "migrations"

runEitherTransaction_ ::
     (HasPostgresConnection m, MonadIO m) => IO (Either e a) -> m (Either e a)
runEitherTransaction_ tx = do
  conn <- getPostgresConn
  liftIO $ begin conn
  liftIO $
    E.catch (runTx conn) (\e -> rollback conn >> E.throw (e :: SomeException))
  where
    runTx conn = do
      result <- tx
      case result of
        Left e -> rollback conn >> return (Left e)
        Right a -> commit conn >> return (Right a)

runTransaction_ :: (HasPostgresConnection m, MonadIO m) => IO a -> m a
runTransaction_ tx = do
  conn <- getPostgresConn
  liftIO $ withTransaction conn tx
