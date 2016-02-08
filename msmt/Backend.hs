module Backend
  ( runBackend
  ) where

import           Control.Monad
import           Database.Persist.Postgresql

import           MSMT.Configuration
import           MSMT.Util

import           Types

runBackend :: ConnectionPool -> Options -> Configuration -> Backend ()
runBackend pool options conf = liftIO $ putStrLn "runBackend"
