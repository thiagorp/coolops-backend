module Main where

import RIO

import Network.Connection (TLSSettings(..))
import Network.HTTP.Client.TLS

import qualified BackgroundJobs.AppJobs as Jobs
import Common.PersistDatabase
import Env

type AppT = RIO Env

app :: AppT ()
app = Jobs.runNext

loopWith :: Env -> IO ()
loopWith env = do
  runRIO env app
  threadDelay 1000000
  loopWith env

main :: IO ()
main = do
  requestManager <- newTlsManagerWith (mkManagerSettings (TLSSettingsSimple True False False) Nothing)
  env <- buildEnv 1 requestManager
  loopWith env
