module MSMT.Database.Subscriptions where



import           Data.Aeson.Types
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
import           MSMT.Util.ErrorT


data SubscriptionError = InvalidRegcode
                       | SubscriptionNotFound


instance Show SubscriptionError where
  show InvalidRegcode = "Invalid Regcode supplied."
  show SubscriptionNotFound = "Could not find a valid subscription for this regcode."

instance ResponseError SubscriptionError where
  errorNum InvalidRegcode       = 101
  errorNum SubscriptionNotFound = 102

getSubscription :: (MSMT m) => Maybe TokenAuth -> ErrorT SubscriptionError m (TokenAuth, Entity Subscription)
getSubscription Nothing = indicate InvalidRegcode
getSubscription (Just auth) = do
  msubscription <- db $ selectFirst [SubscriptionRegcode ==. auth] []
  case msubscription of
    Nothing -> indicate SubscriptionNotFound
    Just subscription -> success (auth, subscription)
