module Slack.UseCases.IntegrateProjectFromOAuth
  ( Params(..)
  , Error(..)
  , call
  ) where

import RIO

import Auth.Domain (CompanyID)
import Common.Database (HasDBTransaction, runTransaction)
import Deployments.Database.Project (getProject)
import Deployments.Domain.Project (Project(..))
import Slack.Api.OAuth
import Slack.Database.ProjectIntegration
import Slack.Domain.ProjectIntegration

data Error
  = CouldNotExchangeCode
  | ProjectNotFound

data Params = Params
  { companyId :: !CompanyID
  , pId :: !Text
  , oAuthCode :: !Text
  }

build :: (MonadIO m) => ProjectID -> WorkspaceTokenResponse -> m ProjectIntegration
build integrationProjectId WorkspaceTokenResponse {..} = do
  integrationId <- genId
  let integrationAccessToken = tokenAccessToken
      integrationWorkspaceName = tokenWorkspaceName
      integrationAppId = tokenAppId
      integrationAppUserId = tokenAppUserId
      integrationInstallerUserId = tokenInstallerUserId
      integrationAuthorizingUserId = tokenAuthorizingUserId
      integrationTeamId = tokenTeamId
      integrationChannelId = tokenChannelId
      integrationScopes = tokenScopes
  return ProjectIntegration {..}

call :: (HasPostgres m, HasDBTransaction m, SlackClientMonad m) => Params -> m (Either Error ProjectIntegration)
call Params {..} = do
  maybeProject <- getProject companyId pId
  case maybeProject of
    Nothing -> return $ Left ProjectNotFound
    Just Project {..} -> do
      eitherResponse <- getWorkspaceToken oAuthCode
      case eitherResponse of
        Left (WrongBodyError _ _) -> return $ Left CouldNotExchangeCode
        Left (UnexpectedHttpStatusError _) -> return $ Left CouldNotExchangeCode
        Right response -> do
          slackProjectIntegration <- build projectId response
          runTransaction $ do
            deleteSlackProjectIntegration projectId
            createSlackProjectIntegration slackProjectIntegration
          return $ Right slackProjectIntegration
