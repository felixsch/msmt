{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Api.Subscriptions where

import           Control.Monad.Trans.Either
import           Data.Aeson.Types
import           Data.ByteString             (ByteString)
import qualified Data.ByteString.Char8       as S8
import           Data.Either
import           Data.Maybe
import           Data.Proxy
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as T
import qualified Data.Vector                 as V
import           Database.Esqueleto          ((^.))
import qualified Database.Esqueleto          as E
import           Database.Persist.Sql
import           GHC.Generics
import           Network.URI
import           Servant.API
import           Servant.Server
import           System.Random


import           MSMT.Api.Auth
import           MSMT.Configuration
import           MSMT.Database.Schema
import           MSMT.Database.Subscriptions
import           MSMT.Monad
import           MSMT.Util
import           MSMT.Util.ErrorT

import           Types

data AnnounceError = ParseAnnounceFailed String

instance Show AnnounceError where
  show (ParseAnnounceFailed _) = "Could not parse your supplied announce. Check your client."

instance ResponseError AnnounceError where
  errorNum (ParseAnnounceFailed _) = 201

data AnnounceResponse = AnnounceResponse
  { login    :: Text
  , password :: Text }
  deriving (Show, Eq, Generic)
instance ToJSON AnnounceResponse

type SubscriptionsAPI = ReqTokenAuth :> "subscriptions" :> "systems" :> ReqBody '[JSON] Value :> Post '[JSON] AnnounceResponse
                   :<|> ReqTokenAuth :> "subscriptions" :> "products.json" :> QueryParam "identifier" Text :> QueryParam "version" Text :> QueryParam "arch" Text :> Get '[JSON] Array


-- /subscriptions/systems ------------------------------------------------------

parseAnnounce :: Text -> Value -> Either String Announce
parseAnnounce regcode = parseEither $ withObject "announce" $ \o -> do
  let announceRegcode = regcode
  announceHostname <- o .:? "hostname"
  announceParent <- o .:? "parent"
  announceHwinfo <- o .:? "hwinfo"
  return Announce{..}

subscriptionAnnounce :: AuthHeader -> Value -> FrontendM AnnounceResponse
subscriptionAnnounce auth value = do
  (regcode, Entity subId sub) <- whenFail (getSubscription $ getToken auth) (lift . responseError)

  conf               <- rtConf <$> ask
  gen_pw             <- liftIO newStdGen
  gen_login          <- liftIO newStdGen


  let password     = T.pack $ genStr conf gen_pw
      login        = T.pack $ "MSMT_" ++ genStr conf gen_login
      mannounce    = parseAnnounce regcode value


  whenLeft $ \err -> debug ("Can not parse announce (" ++ err ++ ")") >> lift $ responseError (ParseAnnounceFailed err)

  db $ insert_ $ fromRight mannounce

  systemId <- db $ insert $ System login password True

  debug $ "Created a new system with id = " ++ show systemId

  -- TODO: Think about how to count the systems. do we really need the syscount?
  db $ update subId [ SubscriptionSystems =. systemId : subscriptionSystems sub ]
  return $ AnnounceResponse login password
  where
    genStr conf gen = take (cfgDefault "frontend" "password-length" 16 conf) $ randomRs ('a', 'z') gen

-- subscriptions/products ------------------------------------------------------

subscriptionProducts :: AuthHeader -> Maybe Text -> Maybe Text -> Maybe Text -> FrontendM Array
subscriptionProducts auth identifier version arch = do
  conf <- rtConf <$> ask
  (regcode, Entity subId sub) <- getSubscription $ getToken auth

  products <- db $ E.select $
                E.from $ \p -> do
                  maybe_ identifier $ \i -> E.where_ $ p ^. ProductIdentifier E.==. E.val i
                  maybe_ version $    \v -> E.where_ $ p ^. ProductVersion E.==. E.val v
                  maybe_ arch $       \a -> E.where_ $ p ^. ProductArch E.==. E.just (E.val a)
                  E.where_ $ p ^. ProductProductClass `E.in_` E.valList (subscriptionProductClasses sub)
                  return p

  V.forM (V.fromList products) $ \p -> do
    repos <- productGetRepos p
    exts  <- productGetExtensions p
    return $ renderProductWithReposAndExt (proxy conf) p repos exts
  where
    proxy conf = cfg' "frontend" "proxy-host" conf :: Text


maybe_ a f = when (isJust a) $ f (fromJust a)


--productToJsonWithRepoAndExts :: [Product] -> FrontendM Object
--productToJsonWithRepoAndExts products =

renderProductWithReposAndExt :: Text -> Entity Product -> [Entity Repository] -> [(Entity Product, [Entity Repository])] -> Value
renderProductWithReposAndExt proxy (Entity pId p) repos exts = object $
  [ "id" .= pId
  , "repositories" .= renderedRepos repos
  ]
  where
    renderedRepos repos = V.map renderRepo $ V.fromList repos
    renderRepo (Entity rId r) = object $
      [ "id"            .= rId
      , "name"          .= repositoryName r
      , "distro_target" .= repositoryDistroTarget r
      , "description"   .= repositoryDescription r
      , "url"           .= replacementForUrl proxy (repositoryUrl r) ]

replacementForUrl :: Text -> Text -> Text
replacementForUrl host url = T.append host $ T.pack $ maybe "invalid-repo/" uriPath (parseURI $ T.unpack url)


productGetExtensions :: Entity Product -> FrontendM [(Entity Product, [Entity Repository])]
productGetExtensions (Entity _ p) = mapM productWithExtension =<< fetchExtentions
  where
    productWithExtension ext = (,) <$> pure ext <*> productGetRepos ext
    fetchExtentions          = db $ E.select $
                                    E.from $ \ext -> do
                                      E.where_ $ ext ^. ProductId `E.in_` E.valList (productExtensions p)
                                      return ext

productGetRepos :: Entity Product -> FrontendM [Entity Repository]
productGetRepos (Entity _ p) = db $ E.select $
                                    E.from $ \repo -> do
                                      E.where_ $ repo ^. RepositoryId `E.in_` E.valList (productRepositories p)
                                      return repo
