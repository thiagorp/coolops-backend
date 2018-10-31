module Common.Database where

import RIO

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Migration

import Common.Config

migrateDb :: IO ()
migrateDb = do
  s <- pgSettings
  conn <- connectPostgreSQL (pgUrl s)
  void $ withTransaction conn (runMigration (ctx conn))
  where
    ctx = MigrationContext cmd False
    cmd = MigrationDirectory "migrations"
