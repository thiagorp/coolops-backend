module Util.FrontendEndpoints where

import RIO

logsPage :: Text -> Text
logsPage deploymentId = "/deployments/" <> deploymentId <> "/logs"
