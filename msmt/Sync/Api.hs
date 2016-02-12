{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}


module Sync.Api where

import Control.Monad.Trans.Either
import           Data.ByteString        (ByteString)
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8  as S8
import           Data.Monoid            ((<>))
import           Data.Text              (Text)
import Data.Aeson
import qualified Data.Text.Encoding     as T
import qualified Network.HTTP.Types as H
import qualified Data.CaseInsensitive as CI
import           Data.Typeable
import           GHC.TypeLits
import           Servant.API            hiding (addHeader)
import           Servant.Client
import           Servant.Common.BaseUrl
import           Servant.Common.Req
import Text.Regex.Posix
import Data.Maybe

import MSMT.Api.Auth

import Types
import Monad


-- Paginated Response ----------------------------------------------------------

instance {-# OVERLAPPING #-} (MimeUnrender ct a) => HasClient (Get (ct ': cts) (Paginated a)) where
  type Client (Get (ct ': cts) (Paginated a)) = EitherT ServantError IO (Paginated a)

  clientWithRoute Proxy req baseurl = do
    (headers, content) <- performRequestCT (Proxy :: Proxy ct) H.methodGet req [200, 203, 204, 302] baseurl
    return $ parseHeader (headers,content)

parseHeader :: ([(H.HeaderName, ByteString)], a) -> Paginated a
parseHeader (headers, resp) = Paginated
  { prv  = prev'
  , nxt  = next'
  , lst = last'
  , rsult = resp }
  where
    findHeader :: ByteString -> Maybe ByteString
    findHeader name = lookup (CI.mk name) headers
    --pagecount = maybe 0 S8.readInt $ findHeader "Per-Page"
    (next', prev', last') = case findHeader "Link" of
      Nothing -> (Nothing, Nothing, Nothing)
      Just b  -> parseLinkHeader b

parseLinkHeader :: ByteString -> (Maybe Int, Maybe Int, Maybe Int)
parseLinkHeader header = (lookup "next" relMap, lookup "prev" relMap, lookup "last" relMap)
  where
    relMap = mapMaybe parseRel links

    parseRel [_, page, rel] = case S8.readInt page of
        Nothing         -> Nothing
        Just (p, _)  -> Just (rel, p)
    parseRel _              = Nothing

    -- this is crap!
    links = filter (\l -> length l == 3) $ matchRels $ S8.split ',' header

    matchRels :: [ByteString] -> [[ByteString]]
    matchRels links = map (\(l :: ByteString) ->
      getAllTextSubmatches $ l =~ ("page=([0-9]+)>; rel=\"(prev|next|last)\"" :: ByteString) :: [ByteString]) links

-- Import API ------------------------------------------------------------------

type SyncAPI = BasicAuth :> "connect" :> "organizations" :> "products" :> QueryParam "page" Int :> Get '[JSON] (Paginated Array)
          :<|> BasicAuth :> "connect" :> "organizations" :> "repositories" :> QueryParam "page" Int :> Get '[JSON] (Paginated Array)
          :<|> BasicAuth :> "connect" :> "organizations" :> "systems" :> QueryParam "page" Int :> Get '[JSON] (Paginated Array)
          :<|> BasicAuth :> "connect" :> "organizations" :> "subscriptions" :> QueryParam "page" Int :> Get '[JSON] (Paginated Array)


syncAPI :: Proxy syncAPI
syncAPI = Proxy
