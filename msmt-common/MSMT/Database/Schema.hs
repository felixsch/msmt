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
share [mkPersist sqlSettings, mkMigrate "migrateAll", mkDeleteCascade sqlSettings] [persistLowerCase|
Product json
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

ProductPredecessor
  productId Int eq
  predecessorId Int eq
  Primary productId

ProductSuccessor
  productId ProductId eq
  successorId ProductId eq

Repository json
  Id Int
  name Text
  distroTarget Text
  description Text
  url Text
  autorefresh Bool
  deriving Show Eq

ProductRepository
  productId ProductId eq
  repositoryId RepositoryId eq
  deriving (Show)

Systems
  Id Int
  login Text
  password Text
  fromScc Bool
  deriving Show Eq
|]
