{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}


module Import.SCC.Api where

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

-- Basic authentification for servant clients ----------------------------------
data BasicAuth
  deriving (Typeable)

data BasicAuthLogin = BasicAuthLogin ByteString ByteString

instance HasClient api => HasClient (BasicAuth :> api) where
   type Client (BasicAuth :> api) = BasicAuthLogin -> Client api

   clientWithRoute Proxy req baseurl login =
     clientWithRoute (Proxy :: Proxy api) (loginRequest login req) baseurl

loginRequest :: BasicAuthLogin -> Req -> Req
loginRequest (BasicAuthLogin user password) req = addHeader "Authorization" auth req
  where
    auth = T.decodeUtf8 ("Basic " <> B64.encode (user <> ":" <> password))

-- Paginated Response ----------------------------------------------------------


data Paginated a = Paginated
  { prev   :: Maybe Int
  , next   :: Maybe Int
  , result :: a }

instance {-# OVERLAPPING #-} (MimeUnrender ct a) => HasClient (Get (ct ': cts) (Paginated a)) where
  type Client (Get (ct ': cts) (Paginated a)) = EitherT ServantError IO (Paginated a)

  clientWithRoute Proxy req baseurl = do
    (headers, content) <- performRequestCT (Proxy :: Proxy ct) H.methodGet req [200, 203, 204, 302] baseurl
    return $ parseHeader (headers,content)

parseHeader :: ([(H.HeaderName, ByteString)], a) -> Paginated a
parseHeader (headers, resp) = Paginated
  { prev  = prev'
  , next  = next'
  , result = resp }
  where
    findHeader :: ByteString -> Maybe ByteString
    findHeader name = lookup (CI.mk name) headers
    --pagecount = maybe 0 S8.readInt $ findHeader "Per-Page"
    (next', prev') = case findHeader "Link" of
      Nothing -> (Nothing, Nothing)
      Just b  -> parseLinkHeader b

parseLinkHeader :: ByteString -> (Maybe Int, Maybe Int)
parseLinkHeader header = (lookup "next" relMap, lookup "prev" relMap)
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
      getAllTextSubmatches $ l =~ ("page=([0-9]+)>; rel=\"(prev|next)\"" :: ByteString) :: [ByteString]) links

-- Import API ------------------------------------------------------------------

type OrganizationsAPI = BasicAuth :> "connect" :> "organizations" :> "products" :> QueryParam "page" Int :> Get '[JSON] (Paginated Array)
                   :<|> BasicAuth :> "connect" :> "organizations" :> "repositories" :> QueryParam "page" Int :> Get '[JSON] (Paginated Array)
                   :<|> BasicAuth :> "connect" :> "organizations" :> "systems" :> QueryParam "page" Int :> Get '[JSON] (Paginated Array)
                   :<|> BasicAuth :> "connect" :> "organizations" :> "subscriptions" :> QueryParam "page" Int :> Get '[JSON] (Paginated Array)


organizationsAPI :: Proxy OrganizationsAPI
organizationsAPI = Proxy

testBaseUrl = BaseUrl Http "localhost" 3000

(testGetProducts :<|> testGetRepositories :<|> testGetSystems :<|> testGetSubscriptions) = client organizationsAPI testBaseUrl

runTest :: EitherT ServantError IO a -> IO a
runTest f = do
  result <- runEitherT f
  case result of
    Left err -> error $ "Error: " ++ show err
    Right r  -> return r
