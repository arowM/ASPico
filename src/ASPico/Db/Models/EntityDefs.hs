{-# LANGUAGE QuasiQuotes #-}

module ASPico.Db.Models.EntityDefs where

import ASPico.Prelude

import Database.Persist ( EntityDef )
import Database.Persist.TH ( persistLowerCase )

import ASPico.Db.Models.Base
    ( CompName, CreatedTime, DeletedTime, PasswordHash, UpdatedTime )

aspicoEntityDefs :: [EntityDef]
aspicoEntityDefs = [persistLowerCase|
  Affiliate
    partner     PartnerId
    advertizer  AdvertizerId
    product     ProductId
    created     CreatedTime

    deriving Eq
    deriving Show
    deriving Typeable

  Conversion
    conversion  ConversionId
    affiliate   AffiliateId
    created     CreatedTime

    deriving Eq
    deriving Show
    deriving Typeable
    |]
