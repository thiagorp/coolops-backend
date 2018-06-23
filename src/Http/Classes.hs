module Http.Classes where

import RIO
import qualified RIO.ByteString.Lazy as LBS

import Network.HTTP.Client

class HasHttpRequestManager m where
  getHttpRequestManager :: m Manager

class (MonadIO m, HasHttpRequestManager m) =>
      HasHttp m
  where
  makeRequest :: Request -> m (Response LBS.ByteString)
  makeRequest request = getHttpRequestManager >>= liftIO . httpLbs request
