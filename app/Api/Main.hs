module Main where

import RIO

import Data.Pool
import Database.PostgreSQL.Simple
import Web.Scotty.Trans (scottyT)

import Common.App (Env(..), run)
import Common.Config (PGSettings(..), appPort, pgSettings)
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
  withResource pool migrateDb
  let runner app = withResource pool $ \conn -> run app (Env conn)
  port <- appPort
  scottyT port runner routes
