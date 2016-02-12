
module MSMT.Monad
  ( MSMT(..)
  , MonadIO(..)
  , lift, when, void
  , say, warn, errors, info, debug
  , forM, forM_
  ) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Database.Persist.Postgresql

import           MSMT.Messages


class (MonadIO m) => MSMT m where
  db :: SqlPersistT IO a -> m a
  sendM :: Message -> m ()

say :: (MSMT m) => String -> m ()
say  = sendM . Say

warn :: MSMT m => String -> m ()
warn = sendM . Warn

errors :: MSMT m => String -> m ()
errors = sendM . Error

info :: MSMT m => String -> m ()
info = sendM . Info

debug :: MSMT m => String -> m ()
debug = sendM . Debug
