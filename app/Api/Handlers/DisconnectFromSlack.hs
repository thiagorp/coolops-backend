module Handlers.DisconnectFromSlack
  ( call
  ) where

import RIO

import Network.HTTP.Types.Status
  ( internalServerError500
  , noContent204
  , notFound404
  )
import Web.Scotty.Trans

import Authorization (AuthenticatedUser(..), User(..))
import qualified Slack.UseCases.DisconnectTeam as App
import Types (WebMonad)

call :: AuthenticatedUser -> WebMonad ()
call (AuthenticatedUser user) = do
  result <- lift $ App.call $ App.Params $ userCompanyId user
  case result of
    Right _ -> status noContent204
    Left App.CouldNotRevokeToken -> status internalServerError500
    Left App.TeamNotFound -> status notFound404
