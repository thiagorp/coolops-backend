module Main where

import RIO
import qualified RIO.ByteString as BS
import qualified RIO.Text as T

import Data.Default.Class (def)
import Data.Pool
import Database.PostgreSQL.Simple
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.Wai (Request)
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.RequestLogger.JSON
import Web.Scotty.Trans (Options(Options), scottyOptsT)

import Common.App
import Common.Config (PGSettings(..), pgSettings, slackApiPort)
import Routes (routes)

acquirePool :: IO (Pool Connection)
acquirePool = do
  s <- pgSettings
  createPool (connection s) close 1 10 (poolSize s)
  where
    connection s = connectPostgreSQL $ pgUrl s

exceptionHandler :: Maybe Request -> SomeException -> IO ()
exceptionHandler r e = do
  BS.putStr ("Error: " <> (T.encodeUtf8 $ T.pack $ show e))
  Warp.defaultOnException r e

settings :: Int -> Warp.Settings
settings port =
  Warp.setPort port $
  Warp.setOnException exceptionHandler $ Warp.defaultSettings

options :: Int -> Options
options = Options 1 . settings

main :: IO ()
main = do
  pool <- acquirePool
  requestManager <- newManager tlsManagerSettings
  logger <-
    mkRequestLogger
      def {outputFormat = CustomOutputFormatWithDetails formatAsJSON}
  let runner app =
        withResource pool $ \conn -> do
          env <- buildEnv conn requestManager
          run app env
  port <- slackApiPort
  scottyOptsT (options port) runner (routes logger)
