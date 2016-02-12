{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}


module MSMT.Api.Auth
  ( AuthHeader
  , TokenAuth
  , ReqTokenAuth, ReqBasicAuth
  , getToken, getBasicAuth
  , BasicAuthLogin(..)
  , BasicAuth(..)
  ) where

import           Data.ByteString        (ByteString)
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8  as S8
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as T

import           Data.Proxy
import           Data.Typeable
import           MSMT.Util
import           Servant.API            hiding (addHeader)
import           Servant.Client
import           Servant.Common.Req
import           Text.Regex.Posix

type AuthHeader = Maybe Text
type TokenAuth  = Text
data BasicAuthLogin = BasicAuthLogin ByteString ByteString

type ReqTokenAuth = Header "Authorization" Text
type ReqBasicAuth = Header "Authorization" Text


getToken :: AuthHeader -> Maybe TokenAuth
getToken Nothing    = Nothing
getToken (Just str) = if T.encodeUtf8 str =~ regex :: Bool
                        then Just $ T.decodeUtf8 $ (getAllTextSubmatches $ T.encodeUtf8 str =~ regex) !! 1
                        else Nothing
    where
      regex = "Token token=\"([0-9a-zA-Z]+)\"" :: ByteString


getBasicAuth :: AuthHeader -> Maybe BasicAuthLogin
getBasicAuth Nothing    = Nothing
getBasicAuth (Just str) = if T.encodeUtf8 str =~ regex :: Bool
                            then checkResult match
                            else Nothing
    where
      regex = "Basic ([0-9a-zA-Z]+):([0-9a-zA-Z])" :: ByteString
      match = getAllTextSubmatches $ T.encodeUtf8 str =~ regex
      checkResult (user:password:[]) = Just $ BasicAuthLogin user password
      checkResult _               = Nothing


-- Basic authentification for servant clients ----------------------------------
data BasicAuth
  deriving (Typeable)

instance HasClient api => HasClient (BasicAuth :> api) where
   type Client (BasicAuth :> api) = BasicAuthLogin -> Client api

   clientWithRoute Proxy req baseurl login =
     clientWithRoute (Proxy :: Proxy api) (loginRequest login req) baseurl

loginRequest :: BasicAuthLogin -> Req -> Req
loginRequest (BasicAuthLogin user password) req = addHeader "Authorization" auth req
  where
    auth = T.decodeUtf8 ("Basic " <> B64.encode (user <> ":" <> password))
