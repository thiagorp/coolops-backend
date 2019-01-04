module Common.PersistDatabase
  ( module Common.PersistDatabase
  , module Database.Esqueleto
  ) where

import RIO

import Database.Esqueleto hiding (selectFirst)
import Database.Esqueleto.Internal.Sql (SqlQuery, SqlSelect, select)

type Db m = ReaderT SqlBackend m

selectFirst :: (MonadIO m, SqlSelect a r) => SqlQuery a -> SqlReadT m (Maybe r)
selectFirst query = do
  res <- select (limit 1 >> query)
  case res of
    (x:_) -> return (Just x)
    _ -> return Nothing
