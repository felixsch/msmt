module Database
  ( initializeDatabase

  ) where

import           Database.Persist.Postgresql

import           MSMT.Configuration
import           MSMT.Util

import           Types


initializeDatabase :: ConnectionPool -> Options -> Configuration -> Backend ()
initializeDatabase pool options conf = liftIO $ putStrLn "initializedDatabase"
