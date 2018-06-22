module Http.Classes where

import RIO
import RIO.ByteString.Lazy (toStrict)

import Network.HTTP.Client

class HasHttpRequestManager m where
  getHttpRequestManager :: m Manager

class (MonadIO m, HasHttpRequestManager m) =>
      HasHttp m
  where
  requestNoBody :: Request -> m (Response ByteString)
  requestNoBody request =
    getHttpRequestManager >>= liftIO . httpLbs request >>=
    return . fmap toStrict
