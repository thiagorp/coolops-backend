module Common.Database where

import RIO

import qualified Control.Exception as E
import Data.Pool (withResource)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Migration

import Env

class HasDBTransaction m where
  runTransaction :: m a -> m a
  runEitherTransaction :: m (Either e a) -> m (Either e a)

instance HasDBTransaction (RIO Env) where
  runTransaction tx = ask >>= runTransaction_ . flip runRIO tx
  runEitherTransaction tx = ask >>= runEitherTransaction_ . flip runRIO tx

class (Monad m, MonadIO m) =>
      HasPostgres m
  where
  getPostgresConn :: (Connection -> m a) -> m a
  runDb :: (ToRow a) => Query -> a -> m Int64
  runDb q a = getPostgresConn $ \conn -> liftIO $ execute conn q a
  runDb' :: (ToRow a) => Query -> a -> m ()
  runDb' q a = void (runDb q a)
  runQuery :: (ToRow a, FromRow b) => Query -> a -> m [b]
  runQuery q a = getPostgresConn $ \conn -> liftIO $ query conn q a
  runQuery_ :: (FromRow b) => Query -> m [b]
  runQuery_ q = getPostgresConn $ \conn -> liftIO $ query_ conn q

instance HasPostgres (RIO Env) where
  getPostgresConn fx = do
    pool <- asks pgConnPool
    withRunInIO $ \run -> withResource pool (run . fx)

migrateDb :: Connection -> IO ()
migrateDb conn = void $ withTransaction conn (runMigration (ctx conn))
  where
    ctx = MigrationContext cmd False
    cmd = MigrationDirectory "migrations"

runEitherTransaction_ :: (HasPostgres m) => IO (Either e a) -> m (Either e a)
runEitherTransaction_ tx =
  getPostgresConn $ \conn -> do
    liftIO $ begin conn
    liftIO $ E.catch (runTx conn) (\e -> rollback conn >> E.throw (e :: SomeException))
  where
    runTx conn = do
      result <- tx
      case result of
        Left e -> rollback conn >> return (Left e)
        Right a -> commit conn >> return (Right a)

runTransaction_ :: (HasPostgres m) => IO a -> m a
runTransaction_ tx = getPostgresConn $ \conn -> liftIO $ withTransaction conn tx
