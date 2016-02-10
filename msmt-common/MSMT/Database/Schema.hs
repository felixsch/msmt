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
  predecessorIds [Int]
  successorIds [Int] Maybe
  extensions [Int]
  repositories [Int]
  deriving Show

Repository
  name Text
  distroTarget Text Maybe
  description Text
  url Text
  enabled Bool
  autorefresh Bool
  deriving Show Eq

System
  login Text
  password Text
  fromScc Bool
  deriving Show Eq
|]


toPersonId :: Int -> Key Product
toPersonId i = toSqlKey (fromIntegral i)
