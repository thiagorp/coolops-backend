module Slack.UseCases.DisconnectTeam
  ( Error(..)
  , Params(..)
  , call
  ) where

import RIO

import Auth.Domain (CompanyID)
import Slack.Api.OAuth (SlackClientMonad, revokeToken)
import Slack.Classes
import Slack.Domain.Team

data Error
  = CouldNotRevokeToken
  | TeamNotFound

newtype Params = Params
  { companyId :: CompanyID
  }

call :: (SlackTeamRepo m, SlackClientMonad m) => Params -> m (Either Error ())
call Params {..} = do
  maybeTeam <- getSlackTeamForCompany companyId
  case maybeTeam of
    Nothing -> return $ Left TeamNotFound
    Just team -> do
      response <- revokeToken (teamAccessToken team)
      case response of
        Left _ -> return $ Left CouldNotRevokeToken
        Right _ -> do
          deleteSlackTeam team
          return $ Right ()
