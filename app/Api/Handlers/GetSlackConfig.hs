module Handlers.GetSlackConfig
  ( call
  ) where

import RIO

import Web.Scotty.Trans

import Authorization (AuthenticatedUser(..), User(..))
import Resources
import Slack.Api.Classes (slackClientId)
import Slack.Classes (getSlackTeamForCompany)
import Types (WebMonad)

call :: AuthenticatedUser -> WebMonad ()
call (AuthenticatedUser (User {..})) = do
  maybeSlackTeam <- lift $ getSlackTeamForCompany userCompanyId
  clientId <- lift $ slackClientId
  json $ slackConfigResource clientId maybeSlackTeam
