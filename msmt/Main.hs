module Main where

import           Control.Concurrent.STM.TQueue
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Data.Maybe
import           Database.Persist.Postgresql
import           System.Exit

import           MSMT.Configuration
import           MSMT.Constants
import           MSMT.Util.ErrorT


import           Monad

import           Cli
import           Sync
import           Types


msmtVersion = "1.0.0"

requiredFields :: [(String, String)]
requiredFields =
  [ "backend" `has` "db-connection"
  , "backend" `has` "api-port"
  , "backend" `has` "api-host" ]

main :: IO ()
main = commandline $ do
  -- parse options  && version
  options <- msmtOptions
  when (version options) $ putStrLn msmtVersion >> exitSuccess

  -- load configuration
  config  <- whenFail (loadConfiguration (getConfig options)) $ \err ->
    die $ "Could not load configuration file: " ++ err

  validateConfiguration config requiredFields

  -- connect to database
  pool <- runStdoutLoggingT $ createPostgresqlPool (cfg' "backend" "db-connection" config)
                                                   (cfgDefault "backend" "db-pool" 3 config)

  case action options of
    "start"   -> putStrLn "Currently not implemented"
    "sync"    -> runSync pool config options
    "status"  -> putStrLn "Currently not implemented"
    "stop"    -> putStrLn "Currently not implemented"
    otherwise -> msmtHelp

  where
    getConfig = fromMaybe configurationPath . configuration
