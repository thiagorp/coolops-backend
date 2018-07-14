module Routes where

import Web.Scotty.Trans

import qualified Handlers.HealthCheck as HealthCheck
import qualified Handlers.Messages as Messages

import Types

routes :: WebRoutes ()
routes = do
  get "/health" HealthCheck.call
  post "/slack/messages" Messages.call
