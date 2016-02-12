{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Sync where

import           Data.Aeson.Types            hiding (Options)
import qualified Data.Vector                 as V
import           Database.Persist.Postgresql
import           Servant.API
import           Servant.Client
import           Servant.Common.BaseUrl

import           Data.ByteString             (ByteString)
import qualified Data.ByteString.Char8       as S8
import           Data.Text                   (Text)
import qualified Data.Text                   as T

import           MSMT.Api.Auth
import           MSMT.Configuration
import           MSMT.Database
import           MSMT.Database.Schema
import           MSMT.Messages
import           MSMT.Monad
import           MSMT.Util
import           MSMT.Util.Workers

import           Monad
import           Sync.Api
import           Sync.Json
import           Sync.Util
import           Types


runSync :: ConnectionPool -> Configuration -> Options -> IO ()
runSync pool conf options = do

  validateConfiguration conf
    [ ("import" , "source")
    , ("import" , "organization")
    , ("import" , "credentials") ]

  chan <- newMessageChannel

  let runtime = Runtime { rtPool = pool
                        , rtConf = conf
                        , rtOpts = options
                        , rtChan = chan }

  stdSubscriber (doDebug options) chan
  runSyncM runtime syncAllEntities


syncAllEntities :: SyncM ()
syncAllEntities = do
  options <- rtOpts <$> ask
  conf    <- rtConf <$> ask

  info "Migrating database..."
  db $ runMigration migrateAll

  -- set source url
  let baseurl = genBaseUrl (cfg' "frontend" "source" conf)
  when (isNothing baseurl) $ liftIO $ die "Invalid Source url specified in configuration."

  -- setup endpoints
  let (pr :<|> repo :<|> sys :<|> sub) = client syncAPI (fromJust baseurl :: BaseUrl)
  let syncFunctions = [ ("products", syncProducts (pr login))
                      , ("subscriptions", syncSubscriptions (sub login))
                      , ("repositories", syncRepositories (repo login))
                      , ("systems", syncSystems (sys login)) ]

  -- start syncing
  info $ "Start syncing from" ++ sourceApi conf
  forM_ (toSync options) $ \sync ->
    case lookup sync syncFunctions of
      Nothing -> warn $ "Invalid entity to sync: " ++ sync
      Just f  -> f

  where
    sourceApi = cfg' "import" "source"

    toSync o = case syncOnly o of
      Just x  -> [x]
      Nothing -> [ "products", "subscriptions"
                 , "repositories", "systems" ]

    login c = BasicAuthLogin (cfg' "frontend" "organization" c) (cfg' "frontend" "credentials" c)


syncProducts :: ApiEndpoint -> SyncM ()
syncProducts productEndpoint = do
  chan <- rtChan <$> ask
  pool <- rtPool <$> ask
  products <- fetchFromEndpoint productEndpoint
  work 10 products $ \_ pa ->
    V.forM_ pa $ \p ->
      case parseMaybe parseProduct p of
        Nothing       -> addMessage chan $ Warn $ "parsing products failed. (id =" ++ show (getOnlyId p) ++ ")"
        Just (key, p) -> withPool pool $ repsert key p

syncSubscriptions :: ApiEndpoint -> SyncM ()
syncSubscriptions endpoint = undefined


syncSystems :: ApiEndpoint -> SyncM ()
syncSystems endpoint = undefined


syncRepositories :: ApiEndpoint -> SyncM ()
syncRepositories endpoint = undefined
