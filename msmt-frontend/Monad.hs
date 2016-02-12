module Monad where

import           Database.Persist.Sql
import           Servant.Server

import           MSMT.Monad
import           MSMT.Util


type FrontendM = ReaderT Runtime (EitherT ServantErr IO)

instance MSMT FrontendM where
  db f = do
    pool <- rtPool <$> ask
    liftIO $ runSqlPool f pool

  sendM msg = do
  chan <- rtChan <$> ask
  liftIO $ atomically $ writeTChan chan msg
