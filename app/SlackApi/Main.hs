module Main where

import RIO

import Data.Pool
import Database.PostgreSQL.Simple
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Web.Scotty.Trans (scottyT)

import Common.App
import Common.Config (PGSettings(..), pgSettings, slackApiPort)
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
  let runner app =
        withResource pool $ \conn -> do
          env <- buildEnv conn requestManager
          run app env
  port <- slackApiPort
  scottyT port runner routes
