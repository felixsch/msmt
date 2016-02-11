{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module MSMT.Api.Auth
  ( AuthHeader
  , TokenAuth, BasicAuth
  , ReqTokenAuth, ReqBasicAuth
  , getToken, getBasicAuth
  ) where

import           Data.ByteString       (ByteString)
import qualified Data.ByteString.Char8 as S8
import           Data.Text             (Text)
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as T
import           Servant.API
import           Text.Regex.Posix

type AuthHeader = Maybe Text
type TokenAuth  = Text
type BasicAuth  = (Text, Text)

type ReqTokenAuth = Header "Authorization" Text
type ReqBasicAuth = Header "Authorization" Text


getToken :: AuthHeader -> Maybe TokenAuth
getToken Nothing    = Nothing
getToken (Just str) = if T.encodeUtf8 str =~ regex :: Bool
                        then Just $ T.decodeUtf8 $ (getAllTextSubmatches $ T.encodeUtf8 str =~ regex) !! 1
                        else Nothing
    where
      regex = "Token token=\"([0-9a-zA-Z]+)\"" :: ByteString


getBasicAuth :: AuthHeader -> Maybe BasicAuth
getBasicAuth Nothing    = Nothing
getBasicAuth (Just str) = if T.encodeUtf8 str =~ regex :: Bool
                            then checkResult match
                            else Nothing
    where
      regex = "Basic ([0-9a-zA-Z]+):([0-9a-zA-Z])" :: ByteString
      match = getAllTextSubmatches $ T.encodeUtf8 str =~ regex
      checkResult (user:password:[]) = Just (T.decodeUtf8 user, T.decodeUtf8 password)
      checkResult _               = Nothing
