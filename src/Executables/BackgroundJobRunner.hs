{-# LANGUAGE NoImplicitPrelude #-}

module Executables.BackgroundJobRunner (run) where

import RIO

import Network.Connection (TLSSettings(..))
import Network.HTTP.Client.TLS

import qualified BackgroundJobs.AppJobs as Jobs
import App

loopWith :: Env -> IO ()
loopWith env = do
  runApp env Jobs.runNext
  threadDelay 100000
  loopWith env

run :: IO ()
run = do
  reqManager <- newTlsManagerWith (mkManagerSettings (TLSSettingsSimple True False False) Nothing)
  env <- buildEnv 1 reqManager
  loopWith env
