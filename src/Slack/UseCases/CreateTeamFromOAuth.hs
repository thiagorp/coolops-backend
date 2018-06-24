module Slack.UseCases.CreateTeamFromOAuth
  ( Params(..)
  , Error(..)
  , call
  ) where

import RIO

import Auth.Domain (CompanyID)
import Slack.Api.OAuth
import Slack.Classes
import Slack.Domain.Team

data Error =
  CouldNotExchangeCode

data Params = Params
  { companyId :: !CompanyID
  , oAuthCode :: !Text
  }

build :: (MonadIO m) => CompanyID -> OAuthTokenResponse -> m Team
build teamCompanyId OAuthTokenResponse {..} = do
  teamId <- genId
  let teamIncomingWebhook =
        IncomingWebhookConfig
          { webhookUrl = incomingWebhookUrl
          , webhookChannel = incomingWebhookChannel
          , webhookConfigurationUrl = incomingWebhookConfigurationUrl
          }
  let teamBotUser = BotUser {..}
  return Team {..}

call :: (SlackTeamRepo m, SlackClientMonad m) => Params -> m (Either Error Team)
call Params {..} = do
  eitherResponse <- getToken oAuthCode
  case eitherResponse of
    Left _ -> return $ Left CouldNotExchangeCode
    Right response -> do
      slackTeam <- build companyId response
      createSlackTeam slackTeam
      return $ Right slackTeam
