module Backend
  ( runBackend
  ) where

import           Control.Monad
import           Database.Persist.Postgresql

import           MSMT.Configuration
import           MSMT.Util

import           Database
import           Types

runBackend :: ConnectionPool -> Options -> Configuration -> IO ()
runBackend pool options conf = do
  initializeDatabase pool
