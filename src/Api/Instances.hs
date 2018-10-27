{-# OPTIONS_GHC -fno-warn-orphans #-}

module Api.Instances
  ( Handler
  ) where

import RIO hiding (Handler)

import Data.Pool (withResource)
import Env
import Yesod.Core

import Api.Routes (Handler)

import qualified Common.Database as DB

instance HasEnv Handler where
  getEnv = getYesod

instance DB.HasPostgres Handler where
  getPostgresConn fx = do
    pool <- pgConnPool <$> getYesod
    withRunInIO $ \run -> withResource pool (run . fx)

instance DB.HasDBTransaction Handler where
  runTransaction tx = do
    runInnerHandler <- handlerToIO
    DB.runTransaction_ (runInnerHandler tx)
  runEitherTransaction tx = do
    runInnerHandler <- handlerToIO
    DB.runEitherTransaction_ (runInnerHandler tx)
