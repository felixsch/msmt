module MSMT.Database
  ( Key(..)
  , Entity (..)
  , module MSMT.Database.Schema
  , withPool
  ) where

import           Database.Persist
import           Database.Persist.Sql

import qualified MSMT.Database.Schema


withPool :: ConnectionPool -> SqlPersistT IO a -> IO a
withPool = flip runSqlPool
