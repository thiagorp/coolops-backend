module Env
  ( Env(..)
  , HasEnv(..)
  , Config.PGSettings(..)
  , Config.KubernetesSettings(..)
  , Config.SlackSettings(..)
  , buildEnv
  ) where

import RIO

import Common.Config as Config
  ( KubernetesSettings(..)
  , PGSettings(..)
  , SlackSettings(..)
  , kubernetesSettings
  , pgSettings
  , slackSettings
  )
import Data.Pool
import qualified Database.PostgreSQL.Simple as PG
import qualified Network.HTTP.Client as Http

data Env = Env
  { pgConnPool :: Pool PG.Connection
  , requestManager :: Http.Manager
  , kubernetesSettings :: Config.KubernetesSettings
  , slackSettings :: Config.SlackSettings
  }

class HasEnv m where
  getEnv :: m Env

instance (Monad m) => HasEnv (ReaderT Env m) where
  getEnv = ask

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
