{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}

module App
  ( App
  , Env(..)
  , Environment.SlackSettings(..)
  , Environment.KubernetesSettings(..)
  , HasEnv(..)
  , buildEnv
  , runApp
  , runDb
  ) where

import RIO

import Common.PersistDatabase
import Common.Config as Environment
  ( PGSettings(..)
  , KubernetesSettings(..)
  , SlackSettings(..)
  , pgSettings
  , kubernetesSettings
  , slackSettings
  )

import Control.Monad.Logger
import qualified Database.Persist.Postgresql as Persist
import qualified Network.HTTP.Client as Http

type App = Db (RIO Env)

data Env = Env
  { persistentConnPool :: Persist.ConnectionPool
  , requestManager :: Http.Manager
  , kubernetesSettings :: Environment.KubernetesSettings
  , slackSettings :: Environment.SlackSettings
  }

class HasEnv m where
  getEnv :: m Env

instance HasEnv (RIO Env) where
  getEnv = ask

instance HasEnv (Db (RIO Env)) where
  getEnv = lift ask

acquirePersistPool :: Int -> IO Persist.ConnectionPool
acquirePersistPool size = do
  s <- Environment.pgSettings
  runNoLoggingT $ Persist.createPostgresqlPool (Environment.pgUrl s) size

buildEnv :: Int -> Http.Manager -> IO Env
buildEnv poolSize reqManager = do
  persistPool <- acquirePersistPool poolSize
  Env persistPool reqManager <$> Environment.kubernetesSettings <*> Environment.slackSettings

runDb :: (HasEnv m, MonadUnliftIO m) => Db m a -> m a
runDb query = do
  pool <- persistentConnPool <$> getEnv
  runSqlPool query pool

runApp :: Env -> App a -> IO a
runApp env = runRIO env . runDb
