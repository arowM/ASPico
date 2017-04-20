{-# LANGUAGE TemplateHaskell #-}

{- |
Module      :  ASPico.Form

Define data types to represent all of the forms that are sent to the API.
-}
module ASPico.Form where

import ASPico.Prelude

import Data.Aeson.TH (defaultOptions, deriveJSON)
import Web.FormUrlEncoded (FromForm, ToForm)

import ASPico.Db
       (AdvertizerId, CreatedTime, CvId, PartnerId, ProductId, URL)

-- ----------------
--  AffiliateForm
-- ----------------

data AffiliateForm = AffiliateForm
  { partner :: PartnerId
  , advertizer :: AdvertizerId
  , product :: ProductId
  , redirectTo :: URL
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

-- ----------------
--  RegisterPushForm
-- ----------------

data RegisterPushForm = RegisterPushForm
  { url :: URL
  } deriving (Data, Eq, Generic, Show, Typeable)

deriveJSON defaultOptions ''RegisterPushForm

instance FromForm RegisterPushForm

instance ToForm RegisterPushForm

data RegisterPushResp = RegisterPushResp
  { pushId :: Text
  } deriving (Data, Eq, Generic, Show, Typeable)

deriveJSON defaultOptions ''RegisterPushResp

instance FromForm RegisterPushResp

instance ToForm RegisterPushResp

-- ----------------
--  RunPushForm
-- ----------------
data RunPushForm = RunPushForm
  { partner :: PartnerId
  , advertizer :: AdvertizerId
  , product :: ProductId
  , conversion :: Maybe CvId
  , created :: CreatedTime
  } deriving (Data, Eq, Generic, Show, Typeable)

deriveJSON defaultOptions ''RunPushForm

instance FromForm RunPushForm

instance ToForm RunPushForm
