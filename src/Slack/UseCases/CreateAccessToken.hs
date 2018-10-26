module Slack.UseCases.CreateAccessToken
  ( Params(..)
  , Error(..)
  , call
  ) where

import RIO

import Auth.Domain (CompanyID)
import Slack.Api.OAuth
import qualified Slack.Database.AccessToken as DB
import Slack.Domain.AccessToken

data Error =
  CouldNotExchangeCode

data Params = Params
  { companyId :: !CompanyID
  , oAuthCode :: !Text
  }

build :: (MonadIO m) => CompanyID -> OAuthTokenResponse -> m AccessToken
build tokenCompanyId OAuthTokenResponse {..} = do
  tokenId <- genId
  return AccessToken {..}

call :: (DB.HasPostgres m, SlackClientMonad m) => Params -> m (Either Error AccessToken)
call Params {..} = do
  eitherResponse <- getToken oAuthCode
  case eitherResponse of
    Left (WrongBodyError _ _) -> return $ Left CouldNotExchangeCode
    Left (UnexpectedHttpStatusError _) -> return $ Left CouldNotExchangeCode
    Right response -> do
      accessToken <- build companyId response
      DB.create accessToken
      return $ Right accessToken
