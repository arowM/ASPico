{-# LANGUAGE QuasiQuotes #-}

module ASPico.Db.Models.EntityDefs where

import Database.Persist (EntityDef)
import Database.Persist.TH (persistLowerCase)

import ASPico.Db.Models.Base
       (AdvertizerId, CreatedTime, CvId, PartnerId, ProductId, URL)

aspicoEntityDefs :: [EntityDef]
aspicoEntityDefs =
  [persistLowerCase|
  Affiliate
    partner     PartnerId
    advertizer  AdvertizerId
    product     ProductId
    redirectTo  URL
    created     CreatedTime

    UniqueAffiliate partner advertizer product
    deriving Eq
    deriving Show
    deriving Typeable

  Conversion
    affiliate   AffiliateId
    conversion  CvId Maybe
    created     CreatedTime

    deriving Eq
    deriving Show
    deriving Typeable
    |]
