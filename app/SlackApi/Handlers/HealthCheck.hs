module Handlers.HealthCheck
  ( call
  ) where

import RIO
import qualified RIO.HashMap as HashMap

import Network.HTTP.Types.Status (ok200)
import Web.Scotty.Trans

import Types (WebHandler)

call :: WebHandler ()
call = do
  status ok200
  json $ HashMap.fromList [("healthy" :: Text, True)]
