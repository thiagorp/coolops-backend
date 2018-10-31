module Deployments.Database.Build
  ( getBuild
  , listBuilds
  ) where

import RIO hiding ((^.), on)

import Database.Esqueleto hiding (selectFirst)

import Common.PersistDatabase
import Model

listBuilds :: (MonadIO m) => (Int64, Int64) -> CompanyId -> Db m [Entity Build]
listBuilds (l, o) companyId =
  select $
  from $ \(b `InnerJoin` p) -> do
    on ((b ^. BuildProjectId) ==. (p ^. ProjectId))
    where_ (p ^. ProjectCompanyId ==. val companyId)
    limit l
    offset o
    return b

getBuild :: (MonadIO m) => CompanyId -> UUID -> Db m (Maybe (Entity Build))
getBuild companyId buildId =
  selectFirst $
  from $ \(b `InnerJoin` p) -> do
    on ((b ^. BuildProjectId) ==. (p ^. ProjectId))
    where_ (p ^. ProjectCompanyId ==. val companyId)
    where_ (b ^. BuildId ==. val (BuildKey buildId))
    return b
