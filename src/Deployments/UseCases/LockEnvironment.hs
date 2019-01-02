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

buildLock :: (MonadIO m) => EnvironmentId -> Text -> m EnvironmentLock
buildLock envId creatorId = do
  now <- liftIO getCurrentTime
  return $ EnvironmentLock
    { environmentLockEnvironmentId = envId
    , environmentLockCreatedBy = creatorId
    , environmentLockReleasedAt = Nothing
    , environmentLockReleasedBy = Nothing
    , environmentLockCreatedAt = now
    , environmentLockUpdatedAt = now
    }

call :: CompanyId -> EnvironmentId -> Text -> App (Either Error EnvironmentLockId)
call cId envId creatorId = runExceptT $ do
  verifyExistingLock envId
  lock <- buildLock envId creatorId
  lockId <- lift $ insert lock
  _ <- lift $ Background.notifyNewEnvironmentLock cId lockId
  return lockId
