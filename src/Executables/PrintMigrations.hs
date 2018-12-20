{-# LANGUAGE NoImplicitPrelude #-}

module Executables.PrintMigrations (run) where

import RIO

import Control.Monad.Logger
import Database.Persist.Postgresql

import Common.Config
import Model

run :: IO ()
run = do
  s <- pgSettings
  runStdoutLoggingT $ withPostgresqlConn (pgUrl s) $ runSqlConn $ printMigration migrateAll

