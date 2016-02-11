module Main where

import           Control.Concurrent
import           Control.Concurrent.STM.TQueue
import           Control.Monad
import           Control.Monad.Logger
import           Database.Persist.Postgresql
import           System.Exit

import           MSMT.Configuration
import           MSMT.Constants
import           MSMT.Messages
import           MSMT.Util
import           MSMT.Util.ErrorT


import           Cli
import           Server
import           Types

msmtFrontendVersion = "1.0.0"

main :: IO ()
main = do
  options <- msmtFrontendOptions
  when (version options) $ print msmtFrontendVersion >> exitSuccess

  config  <- whenFail (loadConfiguration (getConfig options)) $ \err ->
    die $ "Could not load configuration file: " ++ err

  validateConfiguration config $
    [ ("general", "token")
    , ("general", "backend")
    , ("frontend", "port")
    , ("frontend", "db-connection")
    , ("frontend", "proxy-host") ]

  -- connect to database
  pool <- runNoLoggingT $ createPostgresqlPool (cfg' "backend" "db-connection" config)
                                                   (cfgDefault "backend" "db-pool" 3 config)
  messages <- newMessageChannel

  -- output to stdout
  stdSubscriber (doDebug options) messages

  -- output to backendserver
  -- backendSubsriber url messages

  let runtime = Runtime { rtPool = pool
                        , rtChan = messages
                        , rtConf = config
                        , rtOptions = options }

  runFrontendServer runtime



  putStrLn "fo"

  where
    getConfig = fromMaybe configurationPath . configuration
