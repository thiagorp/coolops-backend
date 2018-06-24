module Slack.Database.Team
  ( createSlackTeam
  , getSlackTeam
  ) where

import RIO

import Database.PostgreSQL.Simple

import Auth.Domain (CompanyID)
import Common.Database
import Slack.Domain.Team

createSlackTeam :: (HasPostgres m) => Team -> m ()
createSlackTeam Team {..} = runDb' q values
  where
    q =
      "insert into slack_teams\
        \ (id, company_id, team_name, team_id, incoming_webhook_url, incoming_webhook_channel, incoming_webhook_configuration_url, bot_user_id, bot_access_token, created_at, updated_at)\
        \ values (?, ?, ?, ?, ?, ?, ?, ?, ?, NOW(), NOW())"
    values =
      ( teamId
      , teamCompanyId
      , teamName
      , teamSlackId
      , webhookUrl teamIncomingWebhook
      , webhookChannel teamIncomingWebhook
      , webhookConfigurationUrl teamIncomingWebhook
      , botUserId teamBotUser
      , botUserAccessToken teamBotUser)

getSlackTeam :: (HasPostgres m) => CompanyID -> m (Maybe Team)
getSlackTeam companyId = do
  result <- runQuery q (Only companyId)
  case result of
    [] -> return Nothing
    row:_ -> return $ Just $ buildSlackTeam row
  where
    q =
      "select id, company_id, team_name, team_id, incoming_webhook_url, incoming_webhook_channel, incoming_webhook_configuration_url, bot_user_id, bot_access_token from slack_teams\
        \ where company_id = ? limit 1"

type SlackTeamRow = (ID, CompanyID, Text, Text, Text, Text, Text, Text, Text)

buildSlackTeam :: SlackTeamRow -> Team
buildSlackTeam (teamId, teamCompanyId, teamName, teamSlackId, webhookUrl, webhookChannel, webhookConfigurationUrl, botUserId, botUserAccessToken) =
  let teamIncomingWebhook = IncomingWebhookConfig {..}
      teamBotUser = BotUser {..}
   in Team {..}
