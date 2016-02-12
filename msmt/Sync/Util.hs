module Sync.Util where

import           Data.Aeson.Types
import           Servant.API
import           Servant.Common.BaseUrl

import           Data.ByteString        (ByteString)
import qualified Data.ByteString.Char8  as S8
import           Data.Text              (Text)
import qualified Data.Text              as T

import           MSMT.Api.Auth
import           MSMT.Configuration
import           MSMT.Messages
import           MSMT.Monad
import           MSMT.Util
import           MSMT.Util.ErrorT
import           MSMT.Util.Workers


import           Monad
import           Types


fetchFromEndpoint :: ApiEndpoint -> SyncM (WorkingSet Array)
fetchFromEndpoint endpoint = do

  fetchers    <- cfgDefault "import" "fetchers" 2 . rtConf <$> ask
  queueSize   <- cfgDefault "import" "queue-size" 10 . rtConf <$> ask
  chan        <- rtChan <$> ask
  resultSet   <- newWorkingSet queueSize

  -- fetch first page
  page1 <- liftIO $ runEitherT (endpoint Nothing)
  when (isLeft page1) $ return ()

  -- add page 1 to working queue
  let (Paginated _ _ lst p) = fromRight page1
  addWork resultSet p

  when (isNothing lst) $ return ()

  fetchingSet <- newWorkingSetFixed [2..fromJust lst]

  -- work async async ;)
  fork_ $ do
    work fetchers fetchingSet $ \i page -> do
      result <- runEitherT (endpoint $ Just page)
      case result of
        Left err -> addMessage chan $ Warn $ "Worker " ++ show i ++ ": Fetching page " ++ show page ++ " failed: " ++ show err
        Right (Paginated _ _ _ px) -> addWork resultSet p

    pages <- waitForWorkDone fetchingSet
    addMessage chan $ Info $ "Fetched " ++ show pages ++ " pages from endpoint"
    finish resultSet
  return resultSet



genBaseUrl :: ByteString -> Maybe BaseUrl
genBaseUrl raw = case parseBaseUrl (S8.unpack raw) of
                 Left err  -> Nothing
                 Right url -> Just url
