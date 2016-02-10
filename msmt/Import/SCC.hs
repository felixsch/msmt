module Import.SCC where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TBQueue
import           Control.Monad
import           Control.Monad.Trans.Either
import           Data.Aeson
import           Data.Aeson.Types               (parseMaybe)
import           Data.ByteString                (ByteString)
import qualified Data.ByteString.Char8          as S8
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import qualified Data.Text.Encoding             as T
import qualified Data.Vector                    as V
import           Database.Persist
import           Database.Persist.Sql
import           Servant.API
import           Servant.Client
import           Servant.Common.BaseUrl
import           Servant.Common.Req

import           MSMT.Configuration             hiding (Value)
import           MSMT.Database.Schema
import           MSMT.Database.Util
import           MSMT.Util
import           MSMT.Util.ErrorT
import           MSMT.Util.Workers

import           Import.SCC.Api
import           Import.SCC.Json
import           Types


data ImportError = InvalidSource String
                 | RequestFailed String

instance Show ImportError where
  show (InvalidSource err) = "Invalid source url specified: " ++ err
  show (RequestFailed err) = "Sending request failed: " ++ err



syncSCC :: Runtime -> ErrorT ImportError IO ()
syncSCC (pool, conf, opts) = do
  liftIO $ validateConfiguration conf
    [ ("import" , "source")
    , ("import" , "organization")
    , ("import" , "credentials")]

  url           <- genBaseUrl (cfg' "import" "source" conf)
  productImport <- newWorkingSet queueSize
  reposImport   <- newWorkingSet queueSize
  sysImport     <- newWorkingSet queueSize

  let (products :<|> repositories :<|> systems) = client organizationsAPI url


  -- Import products -----------------------------------------------------------
  info "[sync][products] Start Fetching products for import"
  liftIO $ void $ forkIO $ whenFail (fetch (Just 1) productImport (products login)) $ \err ->
    case err of
      InvalidSource e -> errors e
      -- FIXME: Add retry
      RequestFailed e -> warn e

  info "[sync][products] Starting import workers"

  work workers productImport $ \thread value ->
    case parseMaybe parseProduct value of
      Nothing      -> warn $ "[import][product] worker " ++ show thread ++ ": Could not parse product. skipping... (id = " ++ show (getOnlyId value) ++ ")"
      Just (i, p)  -> withPool pool $ repsert (toSqlKey $ fromIntegral i) p

  total <- waitForWorkDone productImport
  info "[sync][products] Finished syncing products"
  info $ "  :: " ++ show total ++ " products synchronizied"

  -- Import Repositories -------------------------------------------------------

  info "[sync][repositories] Start Fetching repositories for import"
  liftIO $ void $ forkIO $ whenFail (fetch (Just 1) reposImport (repositories login)) $ \err ->
    case err of
      InvalidSource e -> errors e
      -- FIXME: Add retry
      RequestFailed e -> warn e

  info "[sync][repositories] Starting import workers"

  work workers reposImport $ \thread value ->
    case parseMaybe parseRepository value of
      Nothing      -> warn $ "[import][repositories] worker " ++ show thread ++ ": Could not parse repo. skipping... (id = " ++ show (getOnlyId value) ++ ")"
      Just (i, p)  -> withPool pool $ repsert (toSqlKey $ fromIntegral i) p

  total <- waitForWorkDone reposImport
  info "[sync][products] Finished syncing respositories"
  info $ "  :: " ++ show total ++ " repositories synchronizied"

  -- Import Systems ------------------------------------------------------------
  info "[sync][systems] Start Fetching systems for import"
  liftIO $ void $ forkIO $ whenFail (fetch (Just 1) sysImport (systems login)) $ \err ->
    case err of
      InvalidSource e -> errors e
      -- FIXME: Add retry
      RequestFailed e -> warn e

  info "[sync][systems] Starting import workers"

  work workers sysImport $ \thread value ->
    case parseMaybe parseSystem value of
      Nothing      -> warn $ "[import][systems] worker " ++ show thread ++ ": Could not parse systems. skipping... (id = " ++ show (getOnlyId value) ++ ")"
      Just (i, p)  -> withPool pool $ repsert (toSqlKey $ fromIntegral i) p

  total <- waitForWorkDone sysImport
  info "[sync][products] Finished syncing systems"
  info $ "  :: " ++ show total ++ " systems synchronizied"





  where
    transport = if cfgDefault "scc" "use-https" True conf
                  then Https
                  else Http
    workers   = cfgDefault "import" "workers" 5 conf
    queueSize = cfgDefault "import" "queue-size" 10 conf
    login     = BasicAuthLogin (cfg' "import" "organization" conf) (cfg' "import" "credentials" conf)




fetch :: Maybe Int -> WorkingSet Value -> (Maybe Int -> EitherT ServantError IO (Paginated Array)) -> ErrorT ImportError IO ()
fetch Nothing work _         = finish work
fetch page    work getResult = do
  result <- lift $ runEitherT (getResult page)
  case result of
    Left err                          -> indicate $ RequestFailed (show err)
    Right r@(Paginated _ next vector) -> do
      V.mapM_ (addWork work) vector
      fetch next work getResult


genBaseUrl :: ByteString -> ErrorT ImportError IO BaseUrl
genBaseUrl raw = case parseBaseUrl (S8.unpack raw) of
                 Left err  -> indicate $ InvalidSource err
                 Right url -> return url
