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

data AffUrlForm = AffUrlForm
  { partner :: PartnerId
  , advertizer :: AdvertizerId
  , product :: ProductId
  } deriving (Data, Eq, Generic, Show, Typeable)

deriveJSON defaultOptions ''AffUrlForm

instance FromForm AffUrlForm

instance ToForm AffUrlForm

data AffUrlResp = AffUrlResp
  { url :: Text
  } deriving (Data, Eq, Generic, Show, Typeable)

deriveJSON defaultOptions ''AffUrlResp

instance FromForm AffUrlResp

instance ToForm AffUrlResp
