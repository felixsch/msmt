module Cli
  ( msmtFrontendOptions
  ) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Options.Applicative
import           System.Directory
import           System.Environment
import           System.Exit

import           MSMT.Cli

import           Types                  hiding (info)


msmtFrontendOptions :: IO Options
msmtFrontendOptions = execParser optionParser

optionParser :: ParserInfo Options
optionParser = info (helper <*> options) ( fullDesc
                                         <> progDesc descr
                                         <> header title )
  where
    descr = "Subscription management tool (FrontendApi)"
    title = "msmt-frontend - Frontend api service of msmt"
    options = Options
      <$> switch (long "debug" <> help debugHelp)
      <*> switch (long "version" <> help versionHelp)
      <*> optional (strOption (long "connect" <> help connectionHelp))
      <*> optional (strOption (long "token" <> help tokenHelp))
      <*> optional (strOption (long "config" <> help configurationHelp))
    debugHelp         = "Enable debugging output"
    versionHelp       = "Show version and quit"
    connectionHelp    = "Backend API to connect"
    tokenHelp         = "Specify auth token"
    configurationHelp = "Specify configuration file"
