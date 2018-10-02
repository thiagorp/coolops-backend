module Env
  ( Env(..)
  , buildEnv
  ) where

import RIO

import qualified Common.Config as Config
import Data.Pool
import qualified Database.PostgreSQL.Simple as PG
import qualified Network.HTTP.Client as Http

data Env = Env
  { pgConnPool :: Pool PG.Connection
  , requestManager :: Http.Manager
  , kubernetesSettings :: Config.KubernetesSettings
  , slackSettings :: Config.SlackSettings
  }

acquirePool :: Int -> IO (Pool PG.Connection)
acquirePool size = do
  s <- Config.pgSettings
  createPool (connection s) PG.close 1 10 size
  where
    connection s = PG.connectPostgreSQL $ Config.pgUrl s

buildEnv :: Int -> Http.Manager -> IO Env
buildEnv poolSize requestManager = do
  pool <- acquirePool poolSize
  Env pool requestManager <$> Config.kubernetesSettings <*> Config.slackSettings
