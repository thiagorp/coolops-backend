{-# LANGUAGE NoImplicitPrelude #-}

module Deployments.UseCases.LockEnvironment
  ( call
  ) where

import Import

import Control.Monad.Except

import qualified Deployments.Database.EnvironmentLock as Lock

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

call :: EnvironmentId -> Text -> App (Either Error EnvironmentLockId)
call envId creatorId = runExceptT $ do
  verifyExistingLock envId
  lock <- buildLock envId creatorId
  lift $ insert lock
