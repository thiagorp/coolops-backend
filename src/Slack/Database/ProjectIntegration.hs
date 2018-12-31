module Slack.Database.ProjectIntegration
  ( findByProjectId
  ) where

import RIO hiding ((^.))

import Database.Esqueleto hiding (selectFirst)

import Common.PersistDatabase
import Model

findByProjectId :: (MonadIO m) => ProjectId -> Db m (Maybe (Entity SlackProjectIntegration))
findByProjectId projectId =
  selectFirst $
  from $ \i -> do
    where_ (i ^. SlackProjectIntegrationProjectId ==. val projectId)
    return i
