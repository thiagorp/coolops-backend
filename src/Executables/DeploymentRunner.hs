{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Executables.DeploymentRunner (run) where

import Import
import qualified RIO.ByteString as BS

import Network.Connection (TLSSettings(..))
import Network.HTTP.Client.TLS

import Auth.Database
import Deployments.UseCases.RunNextDeployment as App

runNextForCompany :: CompanyId -> App ()
runNextForCompany companyId = do
  result <- App.call companyId
  case result of
    Right _ -> BS.putStr "Ran successfully\n"
    Left App.NoDeploymentToRun -> return ()
    Left App.FailedToRunJob -> BS.putStr "Error on kubernetes\n"
    Left App.MissingEntities -> BS.putStr "Either the Project, Environment or Build for this deployment is missing\n"

app :: App ()
app = do
  companies <- listCompanies
  traverse_ (runNextForCompany . entityKey) companies

loopWith :: Env -> IO ()
loopWith env = do
  runApp env app
  threadDelay 1000000
  loopWith env

run :: IO ()
run = do
  reqManager <- newTlsManagerWith (mkManagerSettings (TLSSettingsSimple True False False) Nothing)
  env <- buildEnv 2 reqManager
  loopWith env

