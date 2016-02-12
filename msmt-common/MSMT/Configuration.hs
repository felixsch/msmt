{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module MSMT.Configuration
  ( loadConfiguration
  , Configuration
  , ConfValue(..)
  , cfgEither, cfg, cfg', cfgDefault
  , validateConfiguration
  , has
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
import           System.Exit
import           System.IO.Error

import           MSMT.Util.ErrorT

type Configuration = Ini

loadConfiguration :: (MonadIO m) => FilePath -> ErrorT String m Configuration
loadConfiguration path = eitherToError =<< liftIO (do
  result <- tryIOError $ readIniFile path
  return $ case result of
    Left err           -> Left (show err)
    Right (Left err)   -> Left err
    Right (Right conf) -> Right conf)

class ConfValue a where
  toValue :: Text -> Either String a

instance ConfValue ByteString where
  toValue = Right . T.encodeUtf8

instance ConfValue Text where
  toValue = Right

instance ConfValue Int where
  toValue conv = case T.decimal conv of
    Left _           -> Left "invalid integer value"
    Right (value, _) -> Right value

instance ConfValue String where
  toValue = Right . T.unpack

instance ConfValue Bool where
  toValue conv
    | T.toCaseFold conv == "true" = Right True
    | T.toCaseFold conv == "false" = Right False
    | otherwise = Left "invalid boolean value"

instance (ConfValue a) => ConfValue (V.Vector a) where
  toValue conv = case mapM toValue (T.words conv) of
    Left err   -> Left $ "invalid list (" ++ err ++ "s)."
    Right list -> Right $ V.fromList list



cfgEither :: ConfValue a => String -> String -> Configuration -> Either String a
cfgEither section key conf = case lookupValue (T.pack section) (T.pack key) conf of
  Left err     -> Left err
  Right result -> toValue result

cfg :: ConfValue a => String -> String -> Configuration -> Maybe a
cfg section key config = either (const Nothing) Just $ cfgEither section key config

cfg' :: ConfValue a => String -> String -> Configuration -> a
cfg' section key config = either failure id $ cfgEither section key config
  where
    failure _ = error $ "Could not find key " ++ section ++ "/" ++ key

cfgDefault :: ConfValue a => String -> String -> a -> Configuration -> a
cfgDefault section key def config = fromMaybe def $ cfg section key config

validateConfiguration :: Configuration -> [(String, String)] -> IO ()
validateConfiguration conf required = forM_ required $ \(section, key) ->
  when (missingValue $ lookupValue (T.pack section) (T.pack key) conf) $
    die $ "Missconfigured configuration detected. `"
      ++ key ++ "` in section `"
      ++ section ++ "` is missing" ++ "\n"
      ++ "Check the your msmt configuration"
  where
    missingValue (Left _)  = True
    missingValue (Right _) = False

has :: String -> String -> (String, String)
has = (,)
