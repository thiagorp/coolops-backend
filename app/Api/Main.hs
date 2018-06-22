module Main where

import RIO

import Data.Pool
import Database.PostgreSQL.Simple
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Web.Scotty.Trans (scottyT)

import Common.App hiding (kubernetesSettings)
import Common.Config (PGSettings(..), appPort, kubernetesSettings, pgSettings)
import Common.Database (migrateDb)
import Routes (routes)

acquirePool :: IO (Pool Connection)
acquirePool = do
  settings <- pgSettings
  createPool (connection settings) close 1 10 (poolSize settings)
  where
    connection settings = connectPostgreSQL $ pgUrl settings

main :: IO ()
main = do
  pool <- acquirePool
  requestManager <- newManager tlsManagerSettings
  k8sSettings <- kubernetesSettings
  withResource pool migrateDb
  let runner app =
        withResource pool $ \conn ->
          run app (Env conn requestManager k8sSettings)
  port <- appPort
  scottyT port runner routes
