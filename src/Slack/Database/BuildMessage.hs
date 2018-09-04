module Slack.Database.BuildMessage
  ( createSlackBuildMessage
  , getSlackBuildMessage
  ) where

import RIO

import Database.PostgreSQL.Simple

import Common.Database
import Slack.Domain.BuildMessage

getSlackBuildMessage :: (HasPostgres m) => BuildID -> m (Maybe BuildMessage)
getSlackBuildMessage buildId = do
  result <- runQuery q (Only buildId)
  case result of
    [] -> return Nothing
    row:_ -> return $ Just (build row)
  where
    q =
      "select id, build_id, slack_message_id from slack_build_messages\
        \ where build_id = ?"

createSlackBuildMessage :: (HasPostgres m) => BuildMessage -> m ()
createSlackBuildMessage BuildMessage {..} = runDb' q values
  where
    q =
      "insert into slack_build_messages\
        \ (id, build_id, slack_message_id, created_at, updated_at)\
        \ values (?, ?, ?, now() at time zone 'utc', now() at time zone 'utc')"
    values = (buildMessageId, buildMessageBuildId, buildMessageSlackMessageId)

type Row = (ID, BuildID, Text)

build :: Row -> BuildMessage
build (buildMessageId, buildMessageBuildId, buildMessageSlackMessageId) = BuildMessage {..}
