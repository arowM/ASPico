{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ASPico.Db.Models
  ( module ASPico.Db.Models
  , module ASPico.Db.Models.Base
  , Entity(..)
  , EntityField(..)
  , GetEntityField(..)
  , Key(..)
  , Unique(..)
  ) where

import ASPico.Prelude

import Data.Aeson.TH (deriveJSON, Options(..), defaultOptions)
import Database.Persist
       (Entity(..), EntityField(..), Key(..), Unique)
import Database.Persist.TH
       (share, mkPersist, sqlSettings, mkMigrate, mpsGenerateLenses)

import ASPico.Db.Models.Base
       (AdvertizerId, CreatedTime(..), CvId, PartnerId, ProductId, URL)
import ASPico.Db.Models.EntityDefs (aspicoEntityDefs)

share
  [mkPersist sqlSettings {mpsGenerateLenses = False}, mkMigrate "migrateAll"]
  aspicoEntityDefs

class GetEntityField record where
  createdEntityField :: EntityField record CreatedTime

instance GetEntityField Affiliate where
  createdEntityField = AffiliateCreated

instance GetEntityField Conversion where
  createdEntityField = ConversionCreated

deriveJSON defaultOptions { fieldLabelModifier = drop 10 } ''Conversion
