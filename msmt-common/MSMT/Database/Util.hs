{-# LANGUAGE FlexibleContexts #-}

module MSMT.Database.Util
  ( withPool

  ) where


import           Control.Monad.Trans.Control
import           Database.Persist.Sql


withPool :: (MonadBaseControl IO m) => ConnectionPool -> SqlPersistT m a -> m a
withPool = flip runSqlPool
