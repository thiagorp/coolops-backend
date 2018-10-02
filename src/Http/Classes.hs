module Http.Classes where

import RIO
import qualified RIO.ByteString.Lazy as LBS

import Network.HTTP.Client

import Env

class (MonadIO m) =>
      HasHttp m
  where
  getHttpRequestManager :: m Manager
  makeRequest :: Request -> m (Response LBS.ByteString)
  makeRequest request = getHttpRequestManager >>= liftIO . httpLbs request

instance HasHttp (RIO Env) where
  getHttpRequestManager = asks requestManager
