module Database.Queries.DeploymentQueuedByLockMessageData
  ( getMessageData
  ) where

import RIO hiding ((^.), isNothing, on)

import Database.Esqueleto hiding (selectFirst)

import Common.PersistDatabase
import Model

type MessageData =
  ( Entity Build
  , Entity Project
  , Entity Environment
  , Entity EnvironmentLock
  , Entity SlackAccessToken
  )

getMessageData :: (MonadIO m) => DeploymentId -> EnvironmentLockId -> Db m (Maybe MessageData)
getMessageData dId elId =
  selectFirst $
    from $ \(d `InnerJoin` e `InnerJoin` b `InnerJoin` p `InnerJoin` el `InnerJoin` sat) -> do
      on $ (sat ^. SlackAccessTokenCompanyId) ==. (p ^. ProjectCompanyId)
      on $ (el ^. EnvironmentLockEnvironmentId) ==. (e ^. EnvironmentId)
      on $ (p ^. ProjectId) ==. (e ^. EnvironmentProjectId)
      on $ (b ^. BuildId) ==. (d ^. DeploymentBuildId)
      on $ (e ^. EnvironmentId) ==. (d ^. DeploymentEnvironmentId)
      where_ $ el ^. EnvironmentLockId ==. val elId
      where_ $ d ^. DeploymentId ==. val dId
      return (b, p, e, el, sat)
