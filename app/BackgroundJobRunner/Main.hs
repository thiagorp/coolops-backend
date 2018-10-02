module Main where

import RIO

import Network.Connection (TLSSettings(..))
import Network.HTTP.Client.TLS

import qualified BackgroundJobs.AppJobs as Jobs
import Env

type AppT = RIO Env

app :: AppT ()
app = Jobs.runNext

loopWith :: Env -> IO ()
loopWith env = do
  runRIO env app
  threadDelay 100
  loopWith env

main :: IO ()
main = do
  requestManager <- newTlsManagerWith (mkManagerSettings (TLSSettingsSimple True False False) Nothing)
  env <- buildEnv 1 requestManager
  loopWith env
