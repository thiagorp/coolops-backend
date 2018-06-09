module Handlers.GetProject
  ( call
  ) where

import RIO

import Network.HTTP.Types.Status (notFound404)
import Web.Scotty.Trans

import Authorization (AuthenticatedUser(..), User(..))
import Deployments.Classes (getProject)
import Resources
import Types (WebMonad)

call :: AuthenticatedUser -> WebMonad ()
call (AuthenticatedUser user) = do
  projectId <- param "id"
  maybeProject <- lift $ getProject (userCompanyId user) projectId
  case maybeProject of
    Just project -> json $ projectResource project
    Nothing -> status notFound404
