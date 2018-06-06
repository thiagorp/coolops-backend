module Common.Database where

import RIO

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Migration

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

migrateDb :: Connection -> IO ()
migrateDb conn = void $ withTransaction conn (runMigration (ctx conn))
  where
    ctx = MigrationContext cmd False
    cmd =
      MigrationCommands
        [MigrationInitialization, MigrationDirectory "migrations"]

runTransaction :: (HasPostgresConnection m, MonadIO m) => IO a -> m a
runTransaction tx = do
  conn <- getPostgresConn
  liftIO $ withTransaction conn tx
