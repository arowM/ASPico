module ASPico.Db.Models.Base where

import ASPico.Prelude

import Data.Aeson (FromJSON, ToJSON)
import Database.Persist (PersistField(..))
import Database.Persist.Sql (PersistFieldSql(..))
import Web.HttpApiData (FromHttpApiData, ToHttpApiData)

-------------------------
-- Aliases for UTCTime --
-------------------------

newtype CreatedTime = CreatedTime
  { unCreatedTime :: UTCTime
  } deriving ( Data
             , Eq
             , FromJSON
             , Generic
             , Ord
             , PersistField
             , PersistFieldSql
             , Show
             , ToJSON
             , Typeable
             )

----------------------
-- Aliases for Text --
----------------------

newtype URL = URL
  { unURL :: Text
  } deriving ( Data
             , Eq
             , FromHttpApiData
             , FromJSON
             , Generic
             , IsString
             , Ord
             , PersistField
             , PersistFieldSql
             , Show
             , ToHttpApiData
             , ToJSON
             , Typeable
             )

------------------
--  Identifiers --
------------------

newtype PartnerId = PartnerId
  { unPartnerId :: Text
  } deriving ( Data
             , Eq
             , FromHttpApiData
             , FromJSON
             , Generic
             , Ord
             , PersistField
             , PersistFieldSql
             , Show
             , ToHttpApiData
             , ToJSON
             , Typeable
             )

newtype AdvertizerId = AdvertizerId
  { unAdvertizerId :: Text
  } deriving ( Data
             , Eq
             , FromHttpApiData
             , FromJSON
             , Generic
             , Ord
             , PersistField
             , PersistFieldSql
             , Show
             , ToHttpApiData
             , ToJSON
             , Typeable
             )

newtype ProductId = ProductId
  { unProductId :: Text
  } deriving ( Data
             , Eq
             , FromHttpApiData
             , FromJSON
             , Generic
             , Ord
             , PersistField
             , PersistFieldSql
             , Show
             , ToHttpApiData
             , ToJSON
             , Typeable
             )

newtype CvId = CvId
  { unCvId :: Text
  } deriving ( Data
             , Eq
             , FromHttpApiData
             , FromJSON
             , Generic
             , Ord
             , PersistField
             , PersistFieldSql
             , Show
             , ToHttpApiData
             , ToJSON
             , Typeable
             )
