module MSMT.Database.Subscriptions where



import           Database.Persist.Sql
import           Servant.Server

import           Data.ByteString       (ByteString)
import qualified Data.ByteString.Char8 as S8
import           Data.Text             (Text)
import qualified Data.Text             as T

import           MSMT.Api.Auth
import           MSMT.Database
import           MSMT.Database.Schema
import           MSMT.Monad
import           MSMT.Util

getSubscription :: (MSMT m) => Maybe TokenAuth -> m (Maybe (TokenAuth, Entity Subscription))
getSubscription Nothing = debug "Invalid token." >> return Nothing
getSubscription (Just auth) = do
  msubscription <- db $ selectFirst [SubscriptionRegcode ==. auth] []
  case msubscription of
    Nothing -> debug ("subscription " ++ T.unpack auth ++ " not found.") >> return Nothing
    Just subscription -> return $ Just (auth, subscription)
