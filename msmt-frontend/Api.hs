{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Api where

import           Data.ByteString       (ByteString)
import qualified Data.ByteString.Char8 as S8
import           Data.Proxy
import           Data.Text             (Text)
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as T
import           Servant.API
import           Text.Regex.Posix

import           Api.Subscriptions

--type TestAPI = "foo" :> "bar" :> Get '[JSON] Bool

type FrontendAPI = SubscriptionsAPI
        --       :<|> TestAPI


frontendAPI :: Proxy FrontendAPI
frontendAPI = Proxy
