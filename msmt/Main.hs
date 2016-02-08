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


import           Backend
import           Cli
import           Database
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
  options <- parseOptions
  when (version options) $ putStrLn msmtVersion >> exitSuccess

  -- load configuration
  config  <- whenFail (loadConfiguration (getConfig options)) $ \err ->
    die $ "Could not load configuration file: " ++ err

  validateConfiguration config requiredFields

  -- connect to database
  -- from now on run in runStdoutLoggingT
  runStdoutLoggingT $ do

    pool     <- createPostgresqlPool (cfg' "backend" "db-connection" config)
                                     (cfgDefault "backend" "db-pool" 3 config)

    case action options of
      "init"    -> initializeDatabase pool options config
      "start"   -> runBackend pool options config
      otherwise -> liftIO showHelp

  where
    getConfig = fromMaybe configurationPath . configuration
