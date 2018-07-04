module Handlers.GetEnvironment
  ( call
  ) where

import RIO

import Network.HTTP.Types.Status (notFound404)
import Web.Scotty.Trans

import Authorization (AuthenticatedUser(..), User(..))
import Deployments.Classes (getEnvironment)
import Resources
import Types (WebMonad)

call :: AuthenticatedUser -> WebMonad ()
call (AuthenticatedUser user) = do
  environmentId <- param "id"
  maybeEnvironment <- lift $ getEnvironment (userCompanyId user) environmentId
  case maybeEnvironment of
    Just environment -> json $ environmentResource environment
    Nothing -> status notFound404
