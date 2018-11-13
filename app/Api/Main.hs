module Main where

import RIO
import qualified RIO.ByteString as BS
import qualified RIO.Text as T

import Data.Default.Class (def)
import Network.Connection (TLSSettings(..))
import Network.HTTP.Client.TLS
import Network.Wai (Middleware, Request)
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.Gzip
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.RequestLogger.JSON

import Api.Handler

import Common.Config (appPort)
import Common.Database
import Env

exceptionHandler :: Maybe Request -> SomeException -> IO ()
exceptionHandler r e = do
  BS.putStr ("Error: " <> T.encodeUtf8 (T.pack $ show e))
  Warp.defaultOnException r e

settings :: Int -> Warp.Settings
settings port = Warp.setPort port $ Warp.setOnException exceptionHandler Warp.defaultSettings

corsMiddleware :: Middleware
corsMiddleware = cors $ const (Just policy)
  where
    policy =
      simpleCorsResourcePolicy
        { corsRequestHeaders = ["content-type", "authorization"]
        , corsMethods = ["GET", "HEAD", "POST", "PATCH", "DELETE"]
        }

mkApp :: Env -> IO Application
mkApp env = do
  logger <- mkRequestLogger def {outputFormat = CustomOutputFormatWithDetails formatAsJSON}
  appPlain <- toWaiAppPlain env
  return $ gzip def $ corsMiddleware $ logger appPlain

main :: IO ()
main = do
  migrateDb
  requestManager <- newTlsManagerWith (mkManagerSettings (TLSSettingsSimple True False False) Nothing)
  env <- buildEnv 1 requestManager
  port <- appPort
  app <- mkApp env
  Warp.runSettings (settings port) app
