module Database.Queries.EnvironmentLockMessageData
  ( MessageData
  , getEnvironmentLockMessageData
  ) where

import RIO hiding ((^.), on)

import Database.Esqueleto hiding (selectFirst)

import Common.PersistDatabase
import Model

type MessageData =
  ( Entity EnvironmentLock
  , Entity Environment
  , Entity Project
  , Entity SlackProjectIntegration
  , Entity SlackAccessToken
  , Maybe (Entity SlackEnvironmentLockMessage)
  )

getEnvironmentLockMessageData ::
  (MonadIO m) =>
  CompanyId ->
  EnvironmentLockId ->
  Db m (Maybe MessageData)
getEnvironmentLockMessageData cId lId =
  selectFirst $
    from $ \(el `InnerJoin` e `InnerJoin` p `InnerJoin` spi `InnerJoin` sat `LeftOuterJoin` selm) -> do
      on $ (selm ?. SlackEnvironmentLockMessageEnvironmentLockId) ==. just (el ^. EnvironmentLockId)
      on $ (sat ^. SlackAccessTokenCompanyId) ==. (p ^. ProjectCompanyId)
      on $ (spi ^. SlackProjectIntegrationProjectId) ==. (p ^. ProjectId)
      on $ (p ^. ProjectId) ==. (e ^. EnvironmentProjectId)
      on $ (e ^. EnvironmentId) ==. (el ^. EnvironmentLockEnvironmentId)
      where_ $ p ^. ProjectCompanyId ==. val cId
      where_ $ el ^. EnvironmentLockId ==. val lId
      return (el, e, p, spi, sat, selm)
