module Types
  ( Options(..)
  , Runtime(..)
  , Paginated(..)
  , SyncError(..)
  , ApiEndpoint
  ) where

import           Control.Monad.Trans.Either
import           Data.Aeson.Types            hiding (Options)
import           Database.Persist.Postgresql
import           Servant.Common.Req

import           MSMT.Configuration
import           MSMT.Messages

-- commandline options
data Options = Options
  { doDebug       :: Bool
  , version       :: Bool
  , connection    :: Maybe String
  , token         :: Maybe String
  , configuration :: Maybe FilePath
  , syncOnly      :: Maybe String
  , action        :: String
} deriving (Show)


data Runtime = Runtime
  { rtPool :: ConnectionPool
  , rtConf :: Configuration
  , rtOpts :: Options
  , rtChan :: MessageChan }

data SyncError = InvalidSource String
               | RequestFailed String

instance Show SyncError where
  show (InvalidSource err) = "Invalid source url specified: " ++ err
  show (RequestFailed err) = "Sending request failed: " ++ err

data Paginated a = Paginated
  { prv   :: Maybe Int
  , nxt   :: Maybe Int
  , lst   :: Maybe Int
  , rsult :: a }

type ApiEndpoint = Maybe Int -> EitherT ServantError IO (Paginated Array)
