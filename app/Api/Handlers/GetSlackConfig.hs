module Handlers.GetSlackConfig
  ( call
  ) where

import RIO

import Web.Scotty.Trans

import Authorization (AuthenticatedUser(..), User(..))
import Resources
import Slack.Api.Classes (slackClientId)
import Slack.Classes (getSlackTeam)
import Types (WebMonad)

call :: AuthenticatedUser -> WebMonad ()
call (AuthenticatedUser (User {..})) = do
  maybeSlackTeam <- lift $ getSlackTeam userCompanyId
  clientId <- lift $ slackClientId
  json $ slackConfigResource clientId maybeSlackTeam
