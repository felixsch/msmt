module MSMT.Util
  -- re-export stuff which is required often
  ( MonadIO(..)
  , lift
  , when
  , forM
  , forM_
  , fromMaybe
  , maybe
  , fork_
  , say, warn, errors, info
  , either ) where

import           Control.Concurrent        (forkIO)
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           System.IO

import           Data.Either
import           Data.Maybe


say :: (MonadIO m) => String -> m ()
say msg = liftIO $ do
  hPutStrLn stdout msg
  hFlush stdin

info :: (MonadIO m) => String -> m ()
info msg = liftIO $ do
  hPutStrLn stdout $ "(II): " ++ msg
  hFlush stdout

warn :: (MonadIO m) => String -> m ()
warn msg = liftIO $ do
  hPutStrLn stderr $ "(WW): " ++ msg
  hFlush stderr

errors :: (MonadIO m) => String -> m ()
errors msg = liftIO $ do
  hPutStrLn stderr $ "(EE): " ++ msg
  hFlush stderr

fork_ :: (MonadIO m) => IO () -> m ()
fork_ = liftIO . void . forkIO
