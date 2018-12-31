module Http.Classes where

import Import
import qualified RIO.ByteString.Lazy as LBS

import Network.HTTP.Client

makeRequest :: Request -> App (Response LBS.ByteString)
makeRequest request = manager >>= liftIO . httpLbs request
  where
    manager = requestManager <$> getEnv
