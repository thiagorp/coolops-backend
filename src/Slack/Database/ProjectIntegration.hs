module Slack.Database.ProjectIntegration
  ( HasPostgres
  , create
  , delete
  , findByProjectId
  , update
  ) where

import RIO

import Database.PostgreSQL.Simple

import Common.Database
import Slack.Domain.ProjectIntegration

create :: (HasPostgres m) => ProjectIntegration -> m ()
create = runDb' q
  where
    q =
      "insert into slack_project_integrations\
        \ (id, project_id, channel_id, channel_name)\
        \ values (?, ?, ?, ?)"

update :: (HasPostgres m) => ProjectIntegration -> m ()
update ProjectIntegration {..} = runDb' q (integrationChannelId, integrationChannelName, integrationId)
  where
    q =
      "update slack_project_integrations set\
        \ (channel_id, channel_name) = (?, ?)\
        \ where id = ?"

findByProjectId :: (HasPostgres m) => ProjectID -> m (Maybe ProjectIntegration)
findByProjectId projectId = do
  result <- runQuery q (Only projectId)
  case result of
    [] -> return Nothing
    row:_ -> return $ Just row
  where
    q =
      "select id, project_id, channel_id, channel_name from slack_project_integrations\
        \ where project_id = ? limit 1"

delete :: (HasPostgres m) => ProjectID -> m ()
delete projectId = runDb' q (Only projectId)
  where
    q = "delete from slack_project_integrations where project_id = ?"
