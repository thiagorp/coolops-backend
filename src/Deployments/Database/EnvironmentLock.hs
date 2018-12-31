{-# LANGUAGE NoImplicitPrelude #-}

module Deployments.Database.EnvironmentLock
  ( module Common.PersistDatabase
  , findForEnvironment
  ) where

import RIO hiding ((^.), isNothing, on)

import Database.Esqueleto hiding (selectFirst)

import Common.PersistDatabase
import Model


findForEnvironment :: (MonadIO m) => EnvironmentId -> Db m (Maybe (Entity EnvironmentLock))
findForEnvironment envId =
  selectFirst $
    from $ \el -> do
      where_ $ el ^. EnvironmentLockEnvironmentId ==. val envId
      where_ $ isNothing $ el ^. EnvironmentLockReleasedAt
      return el
