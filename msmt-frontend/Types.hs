module Types
  ( Options(..)
  , Runtime(..)
  , FrontendM(..)
  , say, info, errors, warn, debug
  , db, ask
  ) where

import           Control.Concurrent.STM
import           Control.Concurrent.STM.TQueue
import           Control.Monad.Reader
import           Control.Monad.Trans.Either
import           Database.Persist.Postgresql
import           Servant.Server

import           MSMT.Configuration
import           MSMT.Database.Util
import           MSMT.Messages
import           MSMT.Util                     hiding (errors, info, say, warn)

data Options = Options
  { doDebug       :: Bool
  , version       :: Bool
  , connection    :: Maybe String
  , token         :: Maybe String
  , configuration :: Maybe FilePath
} deriving (Show)



data Runtime = Runtime
  { rtPool    :: ConnectionPool
  , rtConf    :: Configuration
  , rtOptions :: Options
  , rtChan    :: MessageChan
  }
