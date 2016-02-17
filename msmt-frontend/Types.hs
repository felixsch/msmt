{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

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
import           MSMT.Database
import           MSMT.Messages
import           MSMT.Monad
import           MSMT.Util
import           MSMT.Util.ErrorT

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


instance MSMT FrontendM where
  db f = do
    pool <- rtPool <$> ask
    liftIO $ withPool pool f

  sendM m = do
    chan <- rtChan <$> ask
    addMessage chan m
