{-# LANGUAGE FlexibleContexts #-}

module Import.SCC where

import           Control.Monad
import           Control.Monad.Trans.Either
import           Data.Aeson
import           Data.Aeson.Types           (Parser, parseMaybe)
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Char8      as S8
import qualified Data.Text                  as T
import           Data.Text                 (Text)
import qualified Data.Text.Lazy             as LT
import qualified Data.Text.Lazy.Encoding    as LT
import qualified Data.Vector                as V
import           Database.Persist
import           Database.Persist.Sql
import           Servant.API
import           Servant.Client
import           Servant.Common.BaseUrl
import           Servant.Common.Req

import           MSMT.Configuration         hiding (Value)
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


type ApiEndpoint = Maybe Int -> EitherT ServantError IO (Paginated Array)

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
  subsImport     <- newWorkingSet queueSize

  let (products :<|> repositories :<|> systems :<|> subscriptions) = client organizationsAPI url

  sync "product" workers pool productImport (products login) parseProduct
  sync "repository" workers pool reposImport (repositories login) parseRepository
  sync "system" workers pool sysImport (systems login) parseSystem
  sync "subscription" workers pool subsImport (subscriptions login) parseSubscription
  info "Syncing complete"
  where
    transport = if cfgDefault "scc" "use-https" True conf
                  then Https
                  else Http
    workers   = cfgDefault "import" "workers" 5 conf :: Int
    queueSize = cfgDefault "import" "queue-size" 10 conf
    login     = BasicAuthLogin (cfg' "import" "organization" conf) (cfg' "import" "credentials" conf)


sync :: (ToBackendKey SqlBackend a, MonadIO m) => String -> Int -> ConnectionPool -> WorkingSet Array -> ApiEndpoint -> (Value -> Parser (Key a, a)) -> ErrorT ImportError m ()
sync name workers pool set endpoint parser = do
  info $ "[sync][" ++ name ++ "] Start fetching "++ name ++" form endpoint"
  fork_ $ whenFail (fetchFromEndpoint (Just 1) set endpoint) (warn' . show)

  info' $ "starting worker threads (threadpool size = " ++ show workers ++ ")"
  work workers set $ \_ array -> repsertArray array
  total <- waitForWorkDone set

  info' $ "finished syncing " ++ name
  info $ "  :: " ++ show (total * 50) ++ " " ++ " synchronizied"

  where
    info' msg = info $ "[sync]["++ name ++"] " ++ msg
    warn' msg = warn $ "[sync]["++ name ++"] " ++ msg

    repsertArray array = V.forM_ array $ \value ->
      case parseMaybe parser value of
        Nothing     -> warn' $ "parsing " ++ name ++ " failed. (id =" ++ show (getOnlyId value) ++ ")"
        Just (key, p) -> withPool pool $ repsert key p


fetchFromEndpoint :: Maybe Int -> WorkingSet Array -> ApiEndpoint -> ErrorT ImportError IO ()
fetchFromEndpoint Nothing work _         = finish work
fetchFromEndpoint page    work getResult = do
  result <- lift $ runEitherT (getResult page)
  case result of
    Left err                          -> indicate $ RequestFailed (show err)
    Right r@(Paginated _ next array)   -> addWork work array >> fetchFromEndpoint next work getResult


genBaseUrl :: ByteString -> ErrorT ImportError IO BaseUrl
genBaseUrl raw = case parseBaseUrl (S8.unpack raw) of
                 Left err  -> indicate $ InvalidSource err
                 Right url -> return url
