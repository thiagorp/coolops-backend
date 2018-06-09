module Handlers.ListProjects
  ( call
  ) where

import RIO

import Web.Scotty.Trans

import Authorization (AuthenticatedUser(..), User(..))
import Deployments.Classes (listProjects)
import Resources
import Types (WebMonad)

call :: AuthenticatedUser -> WebMonad ()
call (AuthenticatedUser user) = do
  projects <- lift $ listProjects (userCompanyId user)
  json $ map projectResource projects
