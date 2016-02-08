{-# LANGUAGE OverloadedStrings #-}

module MSMT.Configuration
  ( loadConfiguration
  , Configuration
  , Value(..)
  , HasConfiguration(..)
  , getFromConf
  ) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Data.ByteString           (ByteString)
import qualified Data.ByteString.Char8     as S8
import           Data.Ini
import           Data.Maybe
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as T
import qualified Data.Text.Read            as T
import qualified Data.Vector               as V

import           MSMT.Util.ErrorT

type Configuration = Ini

loadConfiguration :: (MonadIO m) => FilePath -> ErrorT String m Configuration
loadConfiguration path = eitherToError =<< liftIO (readIniFile path)


class Value a where
  toValue :: Text -> Either String a

instance Value ByteString where
  toValue = Right . T.encodeUtf8

instance Value Int where
  toValue conv = case T.decimal conv of
    Left _           -> Left "invalid integer value"
    Right (value, _) -> Right value

instance Value Bool where
  toValue conv
    | T.toCaseFold conv == "true" = Right True
    | T.toCaseFold conv == "false" = Right False
    | otherwise = Left "invalid boolean value"

instance (Value a) => Value (V.Vector a) where
  toValue conv = case mapM toValue (T.words conv) of
    Left err   -> Left $ "invalid list (" ++ err ++ "s)."
    Right list -> Right $ V.fromList list

getFromConf :: Value a => Text -> Text -> Configuration -> Either String a
getFromConf section key conf = case lookupValue section key conf of
  Left err     -> Left err
  Right result -> toValue result

class (Functor m) => HasConfiguration m where
  getEither :: Value a => Text -> Text -> m (Either String a)

  get :: Value a => Text -> Text -> m (Maybe a)
  get section key = either (const Nothing) Just <$> getEither section key

  getDefault :: Value a => Text -> Text -> a -> m a
  getDefault section key def = fromMaybe def <$> get section key
