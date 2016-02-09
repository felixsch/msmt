module Database
  ( initializeDatabase

  ) where

import           Database.Persist.Postgresql

import           MSMT.Configuration
import           MSMT.Database.Schema
import           MSMT.Database.Util
import           MSMT.Util

import           Types



initializeDatabase :: ConnectionPool -> IO ()
initializeDatabase pool = withPool pool $ runMigration migrateAll
