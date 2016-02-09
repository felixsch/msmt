module Types
  ( Options(..)
  , Runtime(..)

  ) where


import           Control.Monad.Logger
import           Database.Persist.Postgresql

import           MSMT.Configuration

-- commandline options
data Options = Options
  { doDebug       :: Bool
  , version       :: Bool
  , connection    :: Maybe String
  , token         :: Maybe String
  , configuration :: Maybe FilePath
  , action        :: String
} deriving (Show)


type Runtime = (ConnectionPool, Configuration, Options)
