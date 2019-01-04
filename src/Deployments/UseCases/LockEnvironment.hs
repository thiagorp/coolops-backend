{-# LANGUAGE NoImplicitPrelude #-}

module Deployments.UseCases.LockEnvironment
  ( call
  ) where

import Import

import Control.Monad.Except

import qualified Deployments.Database.EnvironmentLock as Lock
import qualified BackgroundJobs.AppJobs as Background

data Error
  = EnvironmentAlreadyLocked

verifyExistingLock :: EnvironmentId -> ExceptT Error App ()
verifyExistingLock envId = do
  lock <- lift $ Lock.findForEnvironment envId
  case lock of
    Nothing -> return ()
    Just _ -> throwError EnvironmentAlreadyLocked

mkLock :: (MonadIO m) => EnvironmentId -> Text -> m EnvironmentLock
mkLock envId creatorId = do
  now <- liftIO getCurrentTime
  return $ EnvironmentLock
    { environmentLockEnvironmentId = envId
    , environmentLockCreatedBy = creatorId
    , environmentLockReleasedAt = Nothing
    , environmentLockReleasedBy = Nothing
    , environmentLockCreatedAt = now
    , environmentLockUpdatedAt = now
    }

attachBuildToLock :: EnvironmentLockId -> BuildId -> App ()
attachBuildToLock elId bId = do
  now <- liftIO getCurrentTime
  void $ insert $
    BuildLock
      { buildLockBuildId = bId
      , buildLockEnvironmentLockId = elId
      , buildLockCreatedAt = now
      , buildLockUpdatedAt = now
      }

call ::
  CompanyId
  -> EnvironmentId
  -> Maybe BuildId
  -> Text
  -> App (Either Error EnvironmentLockId)
call cId envId maybeBuildId creatorId = runExceptT $ do
  verifyExistingLock envId
  lock <- mkLock envId creatorId
  lockId <- lift $ insert lock
  lift $ traverse_ (attachBuildToLock lockId) maybeBuildId
  _ <- lift $ Background.notifyNewEnvironmentLock cId lockId
  return lockId
