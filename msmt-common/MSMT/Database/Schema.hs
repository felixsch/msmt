{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}



module MSMT.Database.Schema where



import           Control.Monad
import           Data.Text                   (Text)
import           Data.Time.Clock
import           Database.Persist
import           Database.Persist.Postgresql
import           Database.Persist.Sql
import           Database.Persist.TH


-- Product
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Product
  name Text
  identifier Text
  former Text
  version Text
  releaseType Text Maybe
  arch Text Maybe
  friendlyName Text
  productClass Text
  productFamily Text Maybe
  cpe Text Maybe
  free Bool
  description Text Maybe
  eulaUrl Text
  productType Text
  shortname Text Maybe
  predecessorIds [ProductId]
  successorIds [ProductId] Maybe
  extensions [ProductId]
  repositories [RepositoryId]
  deriving Show

Repository json
  name Text
  distroTarget Text Maybe
  description Text
  url Text
  enabled Bool
  autorefresh Bool
  deriving Show Eq

System json
  login Text
  password Text
  fromScc Bool
  deriving Show Eq

Subscription
  regcode Text
  name Text
  type Text
  status Text
  start UTCTime
  end UTCTime
  sysLimit Int
  sysCount Int
  virtCount Int Maybe
  productClasses [Text]
  families [Text] Maybe
  systems [SystemId]

HardwareInfo json
  sockets Int
  graphics Text Maybe
  arch Text Maybe
  uUID Text Maybe

Announce
  regcode Text
  hostname Text Maybe
  hwinfo HardwareInfo Maybe
  parent Text Maybe
|]


toPersonId :: Int -> Key Product
toPersonId i = toSqlKey (fromIntegral i)
