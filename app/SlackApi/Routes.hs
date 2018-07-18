module Routes where

import Network.Wai (Middleware)
import Web.Scotty.Trans

import qualified Handlers.HealthCheck as HealthCheck
import qualified Handlers.Messages as Messages

import Types

routes :: Middleware -> WebRoutes ()
routes logger = do
  middleware logger
  get "/health" HealthCheck.call
  post "/slack/messages" Messages.call
