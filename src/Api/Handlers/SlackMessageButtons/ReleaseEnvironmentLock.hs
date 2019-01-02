module Api.Handlers.SlackMessageButtons.ReleaseEnvironmentLock
  ( call
  ) where

import Api.Import

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

app :: Text -> EnvironmentLockId -> App ()
app userId lockId = do
  maybeLock <- getReleaseLockData lockId
  case maybeLock of
    Nothing ->
      return ()

    Just (companyId, _) -> do
      release lockId userId
      void $ Background.notifyNewEnvironmentLock companyId lockId

call :: Text -> EnvironmentLockId -> Handler ()
call userId lockId =
  runAppInHandler $ app userId lockId
