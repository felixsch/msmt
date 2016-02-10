module Cli
  ( commandline
  , msmtOptions
  , msmtHelp
  ) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Options.Applicative
import           System.Directory
import           System.Environment
import           System.Exit
import           System.Process

import           MSMT.Cli

import           Types

msmtOptions :: IO Options
msmtOptions = execParser optionParser

optionParser :: ParserInfo Options
optionParser = info (helper <*> options) ( fullDesc
                                         <> progDesc descr
                                         <> header title )
  where
    descr = "Subscription management tool"
    title = "msmt - micro subscription management tool"
    options = Options
      <$> switch (long "debug" <> help debugHelp)
      <*> switch (long "version" <> help versionHelp)
      <*> optional (strOption (long "connect" <> help connectionHelp))
      <*> optional (strOption (long "token" <> help tokenHelp))
      <*> optional (strOption (long "config" <> help configurationHelp))
      <*> strArgument (metavar "action" <> help actionHelp <> value "start")
    debugHelp         = "Enable debugging output"
    versionHelp       = "Show version and quit"
    connectionHelp    = "Backend API to connect"
    tokenHelp         = "Specify auth token"
    configurationHelp = "Specify configuration file"
    actionHelp        = "Action to perfom"


commandline :: IO () -> IO ()
commandline programm = do
  args <- getArgs
  when (null args) msmtHelp

  let (command:args') = args
  if command `elem` subcommands
    then runSubCommand ("msmt-" ++ command) args'
    else withArgs (command:args') programm
  where
    subcommands = ["cli", "proxy", "frontend"]

runSubCommand :: String -> [String] -> IO ()
runSubCommand command args = do
  mpath <- findExecutable command
  case mpath of
    Just path -> callProcess path args
    Nothing   -> die "Could not find msmt subcommand. Is msmt installed correctly and in your $PATH?"


msmtHelp :: IO ()
msmtHelp = showHelp optionParser $
      [ ""
      , "Commands available from the msmt commandline interface"
      , ""
      , "availble from the backend interface:"
      , ""
      , "   init           Initialize a new database"
      , "   start          Start the backend"
      , "   status         Get status of the backend"
      , ""
      , "availble from the cli interface:"
      , "   cli show ...   Show informations about the backend data"
      , ""
      , "   cli --help     Show additional help for the cli commands"
      , "" ]
