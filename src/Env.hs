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
import Control.Monad.Logger
import qualified Database.Persist.Postgresql as Persist
import qualified Network.HTTP.Client as Http

data Env = Env
  { persistentConnPool :: Persist.ConnectionPool
  , requestManager :: Http.Manager
  , kubernetesSettings :: Config.KubernetesSettings
  , slackSettings :: Config.SlackSettings
  }

class HasEnv m where
  getEnv :: m Env

instance (Monad m) => HasEnv (ReaderT Env m) where
  getEnv = ask

instance HasEnv (RIO Env) where
  getEnv = ask

acquirePersistPool :: Int -> IO Persist.ConnectionPool
acquirePersistPool size = do
  s <- Config.pgSettings
  runNoLoggingT $ Persist.createPostgresqlPool (Config.pgUrl s) size

buildEnv :: Int -> Http.Manager -> IO Env
buildEnv poolSize requestManager = do
  persistPool <- acquirePersistPool poolSize
  Env persistPool requestManager <$> Config.kubernetesSettings <*> Config.slackSettings
