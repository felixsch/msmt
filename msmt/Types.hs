module Types
  ( Backend(..)
  , Options(..)
  , Runtime(..)

  ) where


import           Control.Monad.Logger
import           Database.Persist.Postgresql

import           MSMT.Configuration

-- general backend monad type
type Backend = LoggingT IO

-- commandline options
data Options = Options
  { doDebug       :: Bool
  , version       :: Bool
  , connection    :: Maybe String
  , token         :: Maybe String
  , configuration :: Maybe FilePath
  , action        :: String
} deriving (Show)


data Runtime = Runtime
  { connectionPool :: ConnectionPool
  , options        :: Options
  , conf           :: Configuration }
