module Common.PersistDatabase
  ( module Common.PersistDatabase
  , module Database.Persist.Sql
  , HasEnv
  ) where

import RIO

import Database.Esqueleto.Internal.Sql (SqlQuery, SqlSelect, select)
import Database.Persist.Sql hiding ((<=.), (==.), (||.), selectFirst)

import Env

type HasDb m = (HasEnv m, MonadUnliftIO m)

type Db m = ReaderT SqlBackend m

selectFirst :: (MonadIO m, SqlSelect a r) => SqlQuery a -> SqlReadT m (Maybe r)
selectFirst query = do
  res <- select query
  case res of
    (x:_) -> return (Just x)
    _ -> return Nothing

runDb :: (HasDb m) => Db m a -> m a
runDb query = do
  pool <- persistentConnPool <$> getEnv
  runSqlPool query pool
