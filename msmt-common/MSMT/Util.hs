module MSMT.Util
  ( fork_
  , EitherT(..)
  , Either(..)
  , left, right, either
  , fromLeft, fromRight, isRight, isLeft
  , fromMaybe, isNothing, isJust, fromJust
  , (<>), mempty, mappend
  , die
  ) where

import           Control.Concurrent         (forkIO)
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Either
import           System.Exit
import           System.IO

import           Data.Either
import           Data.Maybe
import           Data.Monoid

fork_ :: (MonadIO m) => IO () -> m ()
fork_ = liftIO . void . forkIO

fromLeft :: Either a b -> a
fromLeft (Left a) = a

fromRight :: Either a b -> b
fromRight (Right b) = b
