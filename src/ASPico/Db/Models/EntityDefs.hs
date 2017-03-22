{-# LANGUAGE QuasiQuotes #-}

module ASPico.Db.Models.EntityDefs where

import Database.Persist (EntityDef)
import Database.Persist.TH (persistLowerCase)

import ASPico.Db.Models.Base
       (AdvertizerId, CvId, PartnerId, ProductId, CreatedTime)

aspicoEntityDefs :: [EntityDef]
aspicoEntityDefs =
  [persistLowerCase|
  Affiliate
    partner     PartnerId
    advertizer  AdvertizerId
    product     ProductId
    created     CreatedTime

    deriving Eq
    deriving Show
    deriving Typeable

  Conversion
    affiliate   AffiliateId
    conversion  CvId
    created     CreatedTime

    deriving Eq
    deriving Show
    deriving Typeable
    |]
