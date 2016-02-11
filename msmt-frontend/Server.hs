{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}


module Server where

import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.Trans.Either
import           MSMT.Api.Auth
import           MSMT.Configuration                   hiding (Value)
import           MSMT.Util
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.RequestLogger
import           Servant.API
import           Servant.Server

import           Api
import           Api.Subscriptions
import           Types

import           Data.Aeson.Types


runFrontendServer :: Runtime -> IO ()
runFrontendServer runtime = run port $ logStdout $ app runtime
  where
    port = cfg' "frontend" "port" $ rtConf runtime :: Int


app :: Runtime -> Application
app runtime = serve frontendAPI (frontendServer runtime)

frontendServer :: Runtime -> Server FrontendAPI
frontendServer runtime = enter (toReader runtime) server

toReader :: Runtime -> FrontendM :~> EitherT ServantErr IO
toReader runtime = Nat $ \x -> runReaderT x runtime

server :: ServerT FrontendAPI FrontendM
server = subscriptionAnnounce :<|> subscriptionProducts
