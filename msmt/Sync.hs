{-# LANGUAGE FlexibleContexts  #-}
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
import           MSMT.Util.ErrorT
import           MSMT.Util.Workers

import           Monad
import           Sync.Api
import           Sync.Json
import           Sync.Util
import           Types


runSync :: ConnectionPool -> Configuration -> Options -> IO ()
runSync pool conf options = do

  validateConfiguration conf
    [ ("sync" , "source")
    , ("sync" , "organization")
    , ("sync" , "credentials") ]

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
  let baseurl = genBaseUrl (cfg' "sync" "source" conf)
  when (isNothing baseurl) $ liftIO $ die "Invalid Source url specified in configuration."

  -- setup endpoints
  let (pr :<|> repo :<|> sys :<|> sub) = client syncAPI (fromJust baseurl :: BaseUrl)
  let w = workers conf
      l = login conf
  let syncFunctions = [ ("products", syncProducts w (pr l))
                      , ("subscriptions", syncSubscriptions w (sub l))
                      , ("repositories", syncRepositories w (repo l))
                      , ("systems", syncSystems w (sys l)) ]

  -- start syncing
  info $ "Start syncing from " ++ cfg' "sync" "source" conf
  forM_ (toSync options) $ \sync ->
    case lookup sync syncFunctions of
      Nothing -> warn $ "Invalid entity to sync: " ++ sync
      Just f  -> f

  where
    workers  = cfgDefault "sync" "import-workers" 10
    toSync o = case syncOnly o of
      Just x  -> [x]
      Nothing -> [ "products", "subscriptions"
                 , "repositories", "systems" ]

    login c = BasicAuthLogin (cfg' "sync" "organization" c) (cfg' "sync" "credentials" c)


syncProducts :: Int -> ApiEndpoint -> SyncM ()
syncProducts w productEndpoint = syncEntity "products" w productEndpoint parseProduct asItIs

syncSubscriptions :: Int -> ApiEndpoint -> SyncM ()
syncSubscriptions w subscriptionEndpoint = syncEntity "subscriptions" w subscriptionEndpoint parseSubscription asItIs

syncSystems :: Int -> ApiEndpoint -> SyncM ()
syncSystems w systemEndpoint = syncEntity "systems" w systemEndpoint parseSystem asItIs

syncRepositories :: Int -> ApiEndpoint -> SyncM ()
syncRepositories w repositoryEndpoint = syncEntity "repositories" w repositoryEndpoint parseRepository asItIs

syncEntity :: (ToBackendKey SqlBackend a) => String -> Int -> ApiEndpoint -> (Value -> Parser (Key a, a)) -> (a -> IO a) -> SyncM ()
syncEntity name workers endpoint parser f = do
  info $ "[sync]["++ name ++ "] start syncing " ++ name ++ "..."
  chan <- rtChan <$> ask
  pool <- rtPool <$> ask
  set  <- try $ fetchFromEndpoint endpoint

  case set of
    Left err  -> errors $ show err
    Right workToDo -> do
      work workers workToDo $ \i pa ->
        V.forM_ pa $ \p ->
          case parseMaybe parser p of
            Nothing       -> addMessage chan $ Warn $ "[sync][" ++ name ++ "] parsing "++ name ++" failed. (id =" ++ show (getOnlyId p) ++ ")"
            Just (key, p) -> do
              addMessage chan $ Debug $ "Syncing a " ++ name ++ "..."
              value <- f p
              withPool pool $ repsert key value
      waitForWorkDone workToDo
      info $ "[sync][" ++ name ++"] Importing " ++ name ++ " finished!"

asItIs :: (Monad m) => a -> m a
asItIs = return
