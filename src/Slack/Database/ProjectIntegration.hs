module Slack.Database.ProjectIntegration
  ( HasPostgres
  , createSlackProjectIntegration
  , getSlackIntegrationForProject
  , deleteSlackProjectIntegration
  , getCompanyIdForBuildsProjectIntegration
  ) where

import RIO

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

import Common.Database
import Deployments.Domain.Project (CompanyID)
import Slack.Domain.ProjectIntegration

getCompanyIdForBuildsProjectIntegration :: (HasPostgres m) => Text -> Text -> m (Maybe CompanyID)
getCompanyIdForBuildsProjectIntegration teamId buildId = do
  rows <- runQuery q (teamId, buildId)
  case rows of
    [] -> return Nothing
    Only row:_ -> return $ Just row
  where
    q =
      "select p.company_id from projects p\
        \ join slack_project_integrations spi on p.id = spi.project_id\
        \ join builds b on p.id = b.project_id\
        \ where spi.team_id = ? and b.id = ?"

createSlackProjectIntegration :: (HasPostgres m) => ProjectIntegration -> m ()
createSlackProjectIntegration ProjectIntegration {..} = runDb' q values
  where
    q =
      "insert into slack_project_integrations\
        \ (id, project_id, access_token, workspace_name, app_id, app_user_id, installer_user_id, authorizing_user_id, team_id, channel_id, scopes, created_at, updated_at)\
        \ values (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, now() at time zone 'utc', now() at time zone 'utc')"
    values =
      Row
        integrationId
        integrationProjectId
        integrationAccessToken
        integrationWorkspaceName
        integrationAppId
        integrationAppUserId
        integrationInstallerUserId
        integrationAuthorizingUserId
        integrationTeamId
        integrationChannelId
        integrationScopes

getSlackIntegrationForProject :: (HasPostgres m) => ProjectID -> m (Maybe ProjectIntegration)
getSlackIntegrationForProject projectId = do
  result <- runQuery q (Only projectId)
  case result of
    [] -> return Nothing
    row:_ -> return $ Just $ build row
  where
    q =
      "select id, project_id, access_token, workspace_name, app_id, app_user_id, installer_user_id, authorizing_user_id, team_id, channel_id, scopes from slack_project_integrations\
        \ where project_id = ? limit 1"

deleteSlackProjectIntegration :: (HasPostgres m) => ProjectID -> m ()
deleteSlackProjectIntegration projectId = runDb' q (Only projectId)
  where
    q = "delete from slack_project_integrations where project_id = ?"

data Row = Row
  { rowId :: ID
  , rowProjectId :: ProjectID
  , rowAccessToken :: Text
  , rowWorkspaceName :: Text
  , rowAppId :: Text
  , rowAppUserId :: Text
  , rowInstallerUserId :: Text
  , rowAuthorizingUserId :: Text
  , rowTeamId :: Text
  , rowChannelId :: Text
  , rowScopes :: Scopes
  }

instance FromRow Row where
  fromRow = do
    rowId <- field
    rowProjectId <- field
    rowAccessToken <- field
    rowWorkspaceName <- field
    rowAppId <- field
    rowAppUserId <- field
    rowInstallerUserId <- field
    rowAuthorizingUserId <- field
    rowTeamId <- field
    rowChannelId <- field
    rowScopes <- field
    return Row {..}

instance ToRow Row where
  toRow Row {..} =
    [ toField rowId
    , toField rowProjectId
    , toField rowAccessToken
    , toField rowWorkspaceName
    , toField rowAppId
    , toField rowAppUserId
    , toField rowInstallerUserId
    , toField rowAuthorizingUserId
    , toField rowTeamId
    , toField rowChannelId
    , toField rowScopes
    ]

build :: Row -> ProjectIntegration
build Row {..} =
  let integrationId = rowId
      integrationProjectId = rowProjectId
      integrationAccessToken = rowAccessToken
      integrationWorkspaceName = rowWorkspaceName
      integrationAppId = rowAppId
      integrationAppUserId = rowAppUserId
      integrationInstallerUserId = rowInstallerUserId
      integrationAuthorizingUserId = rowAuthorizingUserId
      integrationTeamId = rowTeamId
      integrationChannelId = rowChannelId
      integrationScopes = rowScopes
   in ProjectIntegration {..}
