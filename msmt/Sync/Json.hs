{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE FlexibleContexts  #-}

module Sync.Json where


import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.Vector          as V
import           Database.Persist.Sql

import           MSMT.Database.Schema



getOnlyId :: Value -> Maybe Int
getOnlyId = parseMaybe . withObject "product" $ (.: "id")

parseProduct :: Value ->  Parser (Key Product, Product)
parseProduct = withObject "product" $ \o -> do
  productId <- o .: "id" :: Parser Int
  productName <- o .: "name"
  productIdentifier <- o .: "identifier"
  productFormer <- o .: "former_identifier"
  productVersion <- o .: "version"
  productReleaseType <- o .: "release_type"
  productArch <- o .: "arch"
  productFriendlyName <- o .: "friendly_name"
  productProductClass <- o .: "product_class"
  productProductFamily <- o .:? "product_family"
  productCpe <- o .: "cpe"
  productFree <- o .: "free"
  productDescription <- o .: "description"
  productEulaUrl <- o .: "eula_url"
  productProductType  <- o .: "product_type"
  productPredecessorIds <- o .: "predecessor_ids"
  productSuccessorIds <- o .:? "successor_ids"
  productShortname <- o .: "shortname"

  extensions <- o .: "extensions"
  productExtensions <- withArray "extensions" extractIds extensions

  repositories <- o .: "repositories"
  productRepositories <- withArray "repositories" extractIds repositories

  return (toKey productId, Product{..})


parseRepository :: Value -> Parser (Key Repository, Repository)
parseRepository = withObject "product" $ \o -> do
  repositoryId <- o .: "id" :: Parser Int
  repositoryName <- o .: "name"
  repositoryDistroTarget <- o .: "distro_target"
  repositoryDescription <- o .: "description"
  repositoryUrl <- o .: "url"
  repositoryEnabled <- o .: "enabled"
  repositoryAutorefresh <- o .: "autorefresh"

  return (toKey repositoryId, Repository{..})

parseSystem :: Value -> Parser (Key System, System)
parseSystem = withObject "system" $ \o -> do
  systemId <- o .: "id" :: Parser Int
  systemLogin <- o .: "login"
  systemPassword <- o .: "password"
  let systemFromScc = True

  return (toKey systemId, System {..})


parseSubscription :: Value -> Parser (Key Subscription, Subscription)
parseSubscription = withObject "subscription" $ \o -> do
  subscriptionId <- o .: "id" :: Parser Int
  subscriptionRegcode <- o .: "regcode"
  subscriptionName <- o .: "name"
  subscriptionType <- o .: "type"
  subscriptionStatus <- o .: "status"
  subscriptionStart <- o .: "starts_at"
  subscriptionEnd <- o .: "expires_at"
  subscriptionSysLimit <- o .: "system_limit"
  subscriptionSysCount <- o .: "systems_count"
  subscriptionVirtCount <- o .: "virtual_count"
  subscriptionProductClasses <- o .: "product_classes"
  subscriptionFamilies <- o .:? "families"

  systems <- o .: "systems"
  subscriptionSystems <- withArray "systems" extractIds systems

  return (toKey subscriptionId, Subscription{..})

toKey :: (ToBackendKey SqlBackend a) => Int -> Key a
toKey = toSqlKey . fromIntegral

extractIds :: (ToBackendKey SqlBackend a) => V.Vector Value -> Parser [Key a]
extractIds array = do
  result <- V.forM array $ withObject "list" (.: "id")

  return $ V.toList (V.map toSqlKey result)





{-
  Id Int
  name Text
  identifier Text
  formerIdentifier Text
  version Text
  releaseType Text Maybe
  arch Text
  friendlyName Text
  productClass Text
  productFamily Text
  cpe Text
  free Bool
  description Text
  eulaUrl Text
  productType Text
  shortname Text
  predecessorIds [Int]
  successorIds [Int]
  extensions [Int]
  repositories [Int] -}
