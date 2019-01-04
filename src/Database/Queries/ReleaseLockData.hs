module Database.Queries.ReleaseLockData
  ( getReleaseLockData
  ) where

import RIO hiding ((^.), on)

import Database.Esqueleto hiding (selectFirst)

import Common.PersistDatabase
import Model

getReleaseLockData :: (MonadIO m) => EnvironmentLockId -> Db m (Maybe (CompanyId, Entity EnvironmentLock))
getReleaseLockData lockId = do
  result <-
    selectFirst $
      from $ \(el `InnerJoin` e `InnerJoin` p) -> do
        on $ (p ^. ProjectId) ==. (e ^. EnvironmentProjectId)
        on $ (e ^. EnvironmentId) ==. (el ^. EnvironmentLockEnvironmentId)
        where_ $ el ^. EnvironmentLockId ==. val lockId
        limit 1
        return (p ^. ProjectCompanyId, el)

  return $ (\(Value cId, el) -> (cId, el)) <$> result
