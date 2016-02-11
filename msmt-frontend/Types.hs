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



type FrontendM = ReaderT Runtime (EitherT ServantErr IO)

say :: String -> FrontendM ()
say = sendMessage . Say

warn :: String -> FrontendM ()
warn = sendMessage . Warn

errors :: String -> FrontendM ()
errors = sendMessage . Error

info :: String -> FrontendM ()
info = sendMessage . Info

debug :: String -> FrontendM ()
debug = sendMessage . Debug

sendMessage :: Message -> FrontendM ()
sendMessage msg = do
  chan <- rtChan <$> ask
  liftIO $ atomically $ writeTChan chan msg

db :: SqlPersistT IO a -> FrontendM a
db f = do
  pool <- rtPool <$> ask
  liftIO $ runSqlPool f pool
