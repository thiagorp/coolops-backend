module Slack.UseCases.CreateAccessToken
  ( Params(..)
  , Error(..)
  , call
  ) where

import Import

import Slack.Api.OAuth
import qualified Slack.Database.AccessToken as DB

data Error =
  CouldNotExchangeCode

data Params = Params
  { companyId :: !CompanyId
  , oAuthCode :: !Text
  }

build :: CompanyId -> OAuthTokenResponse -> App SlackAccessToken
build slackAccessTokenCompanyId OAuthTokenResponse {..} = do
  now <- liftIO getCurrentTime
  let slackAccessTokenCreatedAt = now
  let slackAccessTokenUpdatedAt = now
  return SlackAccessToken {..}

call :: Params -> App (Either Error SlackAccessToken)
call Params {..} = do
  eitherResponse <- getToken oAuthCode
  case eitherResponse of
    Left (WrongBodyError _ _) -> return $ Left CouldNotExchangeCode
    Left (UnexpectedHttpStatusError _) -> return $ Left CouldNotExchangeCode
    Right response -> do
      accessToken <- build companyId response
      DB.create accessToken
      return $ Right accessToken
