{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Monad
  ( BackendM(..)
  , SyncM(..)
  , runSyncM
  , ask
  ) where

import           Control.Monad.Reader
import           Database.Persist.Sql
import           Servant.Server

import           MSMT.Messages
import           MSMT.Monad
import           MSMT.Util

import           Types


type BackendM = ReaderT Runtime (EitherT ServantErr IO)

type SyncM = ReaderT Runtime IO

instance MSMT BackendM where
  db f = do
    pool <- rtPool <$> ask
    liftIO $ runSqlPool f pool

  sendM msg = do
    chan <- rtChan <$> ask
    addMessage chan msg

instance MSMT SyncM where
  db f = do
    pool <- rtPool <$> ask
    liftIO $ runSqlPool f pool

  sendM msg = do
    chan <- rtChan <$> ask
    addMessage chan msg

instance MSMT (EitherT SyncError SyncM) where
  db    = lift . db
  sendM = lift . sendM

runSyncM :: Runtime -> SyncM a -> IO a
runSyncM rt f = runReaderT f rt
