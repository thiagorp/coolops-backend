module Deployments.UseCases.ReleaseEnvironmentLock
  ( call
  ) where

import Import

import qualified BackgroundJobs.AppJobs as Background
import Database.Queries.ReleaseLockData (getReleaseLockData)

release :: EnvironmentLockId -> Text -> App ()
release lockId userId = do
  now <- liftIO getCurrentTime
  update
    lockId
    [ EnvironmentLockUpdatedAt =. now
    , EnvironmentLockReleasedAt =. Just now
    , EnvironmentLockReleasedBy =. Just userId
    ]

call :: Text -> EnvironmentLockId -> App ()
call userId lockId = do
  maybeLock <- getReleaseLockData lockId
  case maybeLock of
    Nothing ->
      return ()

    Just (companyId, _) -> do
      release lockId userId
      void $ Background.notifyNewEnvironmentLock companyId lockId
