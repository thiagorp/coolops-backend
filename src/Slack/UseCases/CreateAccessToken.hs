module Slack.UseCases.CreateAccessToken
  ( module Model
  , Params(..)
  , Error(..)
  , call
  ) where

import RIO

import Model
import Slack.Api.OAuth
import qualified Slack.Database.AccessToken as DB

data Error =
  CouldNotExchangeCode

data Params = Params
  { companyId :: !CompanyId
  , oAuthCode :: !Text
  }

build :: (MonadIO m) => CompanyId -> OAuthTokenResponse -> m SlackAccessToken
build slackAccessTokenCompanyId OAuthTokenResponse {..} = do
  now <- liftIO getCurrentTime
  let slackAccessTokenCreatedAt = now
  let slackAccessTokenUpdatedAt = now
  return SlackAccessToken {..}

call :: (DB.HasDb m, SlackClientMonad m) => Params -> m (Either Error SlackAccessToken)
call Params {..} = do
  eitherResponse <- getToken oAuthCode
  case eitherResponse of
    Left (WrongBodyError _ _) -> return $ Left CouldNotExchangeCode
    Left (UnexpectedHttpStatusError _) -> return $ Left CouldNotExchangeCode
    Right response -> do
      accessToken <- build companyId response
      DB.runDb $ DB.create accessToken
      return $ Right accessToken
