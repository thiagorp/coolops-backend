module Main where

import RIO

import Control.Monad.Logger
import Database.Persist.Postgresql

import Common.Config
import Model

main :: IO ()
main = do
  s <- pgSettings
  runStdoutLoggingT $ withPostgresqlConn (pgUrl s) $ runSqlConn $ printMigration migrateAll
