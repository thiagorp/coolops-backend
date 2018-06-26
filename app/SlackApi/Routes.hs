module Routes where

import Web.Scotty.Trans

import qualified Handlers.Messages as Messages

import Types

routes :: WebRoutes ()
routes = do
  post "/slack/messages" Messages.call
