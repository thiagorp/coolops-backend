module Main where

import RIO

import Database.PostgreSQL.Simple
import Network.Connection (TLSSettings(..))
import Network.HTTP.Client.TLS

import qualified BackgroundJobs.AppJobs as Jobs
import Common.App
import Common.Config (PGSettings(..), pgSettings)

app :: AppT ()
app = Jobs.runNext

loopWith :: Env -> IO ()
loopWith env = do
  run app env
  threadDelay 100
  loopWith env

main :: IO ()
main = do
  conn <- pgSettings >>= connectPostgreSQL . pgUrl
  requestManager <- newTlsManagerWith (mkManagerSettings (TLSSettingsSimple True False False) Nothing)
  env <- buildEnv conn requestManager
  loopWith env
