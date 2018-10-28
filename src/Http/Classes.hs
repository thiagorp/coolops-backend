module Http.Classes where

import RIO
import qualified RIO.ByteString.Lazy as LBS

import Network.HTTP.Client

import Env

makeRequest :: (HasEnv m, MonadIO m) => Request -> m (Response LBS.ByteString)
makeRequest request = manager >>= liftIO . httpLbs request
  where
    manager = requestManager <$> getEnv
