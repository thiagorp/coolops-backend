module Routes where

import RIO

import Web.Scotty.Trans

import Types

routes :: WebRoutes ()
routes = do
  get "/oauth/callback" undefined
