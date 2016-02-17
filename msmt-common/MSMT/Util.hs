{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module MSMT.Util
  ( fork_
  , EitherT(..)
  , Either(..)
  , left, right, either
  , fromLeft, fromRight, isRight, isLeft
  , fromMaybe, isNothing, isJust, fromJust, whenLeft
  , (<>), mempty, mappend
  , die
  , ResponseError(..)
  ) where

import           Control.Concurrent         (forkIO)
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Either
import           Data.Aeson
import           Data.Aeson.Types
import           Servant.Server
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


whenLeft x f = when (isLeft x) (f $ fromLeft x)


class Show a => ResponseError a where
  errorNum :: a -> Int

  errorMsg :: a -> String
  errorMsg = show

  prefResponse :: a -> ServantErr
  prefResponse _ = err404

  responseError :: (Monad m) => a -> EitherT ServantErr m b
  responseError err = left $ (prefResponse err) { errBody = encode $ object
      [ "error" .= errorNum err
      , "message" .= errorMsg err ] }
