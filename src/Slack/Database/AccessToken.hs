module Slack.Database.AccessToken
  ( HasPostgres
  , create
  , findByProjectId
  , findByTeamId
  ) where

import RIO

import Database.PostgreSQL.Simple

import Common.Database
import qualified Deployments.Domain.Project as Project
import Slack.Domain.AccessToken

create :: (HasPostgres m) => AccessToken -> m ()
create = runDb' q
  where
    q =
      "insert into slack_access_tokens\
        \ (id, company_id, team_name, team_id, scopes, user_access_token, bot_access_token, bot_user_id)\
        \ values (?, ?, ?, ?, ?, ?, ?, ?)"

findByTeamId :: (HasPostgres m) => Text -> m (Maybe AccessToken)
findByTeamId tId = do
  result <- runQuery q (Only tId)
  case result of
    [] -> return Nothing
    row:_ -> return $ Just row
  where
    q =
      "select id, company_id, team_name, team_id, scopes, user_access_token, bot_access_token, bot_user_id from slack_access_tokens\
        \ where team_id = ?"

findByProjectId :: (HasPostgres m) => Project.ID -> m (Maybe AccessToken)
findByProjectId pId = do
  result <- runQuery q (Only pId)
  case result of
    [] -> return Nothing
    row:_ -> return $ Just row
  where
    q =
      "select sat.id, sat.company_id, sat.team_name, sat.team_id, sat.scopes, sat.user_access_token, sat.bot_access_token, sat.bot_user_id from slack_access_tokens sat\
        \ inner join projects p on p.company_id = sat.company_id\
        \ where p.id = ?"
