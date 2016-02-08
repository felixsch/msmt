import           Control.Monad.IO.Class      (liftIO)
import           Data.ByteString             (ByteString)
import qualified Data.ByteString.Char8       as S8
import qualified Data.ByteString.Lazy        as L
import           Data.ByteString.Lazy.Char8  ()
import qualified Network.HTTP.Types          as H
import qualified Network.Wai                 as W
import           Prelude                     hiding (FilePath)

import           Blaze.ByteString.Builder    (toByteString)

import           Data.Text                   (Text)
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as TE

import qualified Data.HashTable.IO           as Hash
import qualified Data.Map                    as M
import qualified Data.Vector.Unboxed.Mutable as V


import           Network.HTTP.Date           (epochTimeToHTTPDate,
                                              formatHTTPDate, parseHTTPDate)


-- ttl = Time on insert + proxyCachTTL
type TTL = Int

data ProxyState = ProxyState
  { proxyCacheTable   :: Hash.BasicHashTable Int TTL
  , proxyBlockedTable :: V.IOVector Int }

data ProxySettings  = ProxySettings
  { proxyPath                :: String
  , proxyCacheTTL            :: Int
  , proxyCachingRepositories :: M.Map ByteString Int }


-- create a wai application
  -- on response:
    -- hash the request path
      -- IS in hashtable
        -- check if ttl is lower than now
          -- cache IS valid
            -- serve the cache
          -- cache IS NOT valid
            -- remove key from cache
            -- add key to blocked
            -- delete file
            -- remove from blocked
            -- goto [1]
      -- [1] IS NOT in hashtable
        -- check if hash is in blocked table
          -- hash IS in blocked table
            -- redirect request to original source
          -- hash IS NOT in blocked table
            -- add hash to blocked table
            -- download file to cache (name: hashed path)
            -- redirect download as response
            -- when finished
            -- add hash to cache hashtable
            -- remove hash from blocked tablec
proxy :: ProxyState -> ProxySettings -> W.Application
proxy state settings request respond = undefined

main :: IO ()
main = do
  -- load configuration from ini
  -- start backend api
  -- create tables
  -- if data in path exists load from paths
  -- start the garbage collector
  -- start updater
  -- run proxy warp application

  return ()

backend :: String -> IO ()
backend = undefined

collectGarbage :: ProxyState -> ProxySettings -> IO ()
collectGarbage = undefined

updateMeta :: ProxyState -> ProxySettings -> IO ()
updateMeta = undefined
