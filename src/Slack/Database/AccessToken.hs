module Slack.Database.AccessToken
  ( module Common.PersistDatabase
  , create
  , findByProjectId
  , findByTeamId
  ) where

import RIO hiding ((^.), on)

import Database.Esqueleto hiding (selectFirst)

import Common.PersistDatabase
import Model

create :: (MonadIO m) => SlackAccessToken -> Db m ()
create = void . insertEntity

findByTeamId :: (MonadUnliftIO m) => Text -> Db m [Entity SlackAccessToken]
findByTeamId tId =
  select $
  from $ \a -> do
    where_ (a ^. SlackAccessTokenTeamId ==. val tId)
    return a

findByProjectId :: (MonadIO m) => ProjectId -> Db m (Maybe (Entity SlackAccessToken))
findByProjectId pId =
  selectFirst $
  from $ \(a `InnerJoin` p) -> do
    on ((a ^. SlackAccessTokenCompanyId) ==. (p ^. ProjectCompanyId))
    where_ (p ^. ProjectId ==. val pId)
    return a
