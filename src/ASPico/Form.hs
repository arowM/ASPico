{-# LANGUAGE TemplateHaskell #-}

{- |
Module      :  ASPico.Form

Define data types to represent all of the forms that are sent to the API.
-}
module ASPico.Form where

import ASPico.Prelude

import Data.Aeson.TH (defaultOptions, deriveJSON)
import Web.FormUrlEncoded (FromForm, ToForm)

import ASPico.Db (AdvertizerId, PartnerId, ProductId)

data AffiliateForm = AffiliateForm
  { partner :: PartnerId
  , advertizer :: AdvertizerId
  , product :: ProductId
  } deriving (Data, Eq, Generic, Show, Typeable)

deriveJSON defaultOptions ''AffiliateForm

instance FromForm AffiliateForm

instance ToForm AffiliateForm

data AffiliateResp = AffiliateResp
  { affId :: Text
  } deriving (Data, Eq, Generic, Show, Typeable)

deriveJSON defaultOptions ''AffiliateResp

instance FromForm AffiliateResp

instance ToForm AffiliateResp
