module BackgroundJobs.Database
  ( module App
  , module Model
  , Entity(..)
  , getNextJob
  ) where

import RIO hiding ((^.), isNothing, on)

import Database.Esqueleto hiding (selectFirst)

import App
import Common.PersistDatabase
import Model

getNextJob :: (MonadIO m) => Db m (Maybe (Entity BackgroundJob))
getNextJob = do
  now <- liftIO getCurrentTime
  selectFirst $
    from $ \bj -> do
      where_ (isNothing (bj ^. BackgroundJobNextRetry) ||. (bj ^. BackgroundJobNextRetry <=. just (val now)))
      where_ (isNothing (bj ^. BackgroundJobFinishedAt))
      orderBy [asc (bj ^. BackgroundJobCreatedAt)]
      locking ForUpdateSkipLocked
      return bj
