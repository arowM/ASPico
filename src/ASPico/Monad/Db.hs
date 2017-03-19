module ASPico.Monad.Db where

import ASPico.Prelude

import Database.Persist.Sql
       (Filter, PersistEntity(..), PersistRecordBackend, PersistStoreRead,
        SqlBackend, Update, (=.), (==.), entityVal, get, getBy,
        insertEntity, selectFirst, selectList, update)

import ASPico.Db
       (Company(..), CompanyUser(..), CompName, CreatedTime(..),
        DeletedTime(..), Entity(Entity), EntityField(..),
        GetEntityField(..), Key, Password, Site(..), Unique(..),
        UpdatedTime(..), deletedEntityField, runDb, runDbCurrTime)
import ASPico.Monad.Auth (MonadASPicoAuth(authHashPass))
import ASPico.Monad.Base (ASPicoM)
import ASPico.Password
       (PasswordCheck(PasswordCorrect, PasswordIncorrect), checkPassword)

---------------------------------------------------
-- Typeclass and Instance with Generic Functions --
---------------------------------------------------
class Monad m =>
      MonadASPicoDb m where

  -- | Create a new record.
  dbCreate
    :: PersistRecordBackend record SqlBackend
    => (UTCTime -> m record) -> m (Entity record)

  -- | Get all entities matching a 'Filter'.  Filters out deleted records.
  dbGetEntitiesBy
    :: (GetEntityField record, PersistRecordBackend record SqlBackend)
    => [Filter record] -> m [Entity record]

  -- | Just like Persistent's 'getEntity'. Does not filter out deleted records.
  dbGetEntityAll
    :: PersistRecordBackend record SqlBackend
    => Key record -> m (Maybe (Entity record))

  -- | Similar to Persistent's 'getEntity'. Filters out deleted records
  dbGetEntity
    :: (GetEntityField record, PersistRecordBackend record SqlBackend)
    => Key record -> m (Maybe (Entity record))

  -- | Just like Persistent's 'getBy'.  Does not filter out deleted records.
  dbGetEntityBy
    :: PersistRecordBackend record SqlBackend
    => Unique record -> m (Maybe (Entity record))

instance MonadASPicoDb ASPicoM where

  dbCreate
    :: PersistRecordBackend record SqlBackend
    => (UTCTime -> ASPicoM record) -> ASPicoM (Entity record)
  dbCreate recordCreator =
    runDbCurrTime $ \currTime -> do
      record <- lift $ recordCreator currTime
      insertEntity record

  dbGetEntitiesBy
    :: (GetEntityField record, PersistRecordBackend record SqlBackend)
    => [Filter record] -> ASPicoM [Entity record]
  dbGetEntitiesBy filters =
    runDb $ selectList ((deletedEntityField ==. Nothing) : filters) []

  dbGetEntityAll
    :: PersistRecordBackend record SqlBackend
    => Key record -> ASPicoM (Maybe (Entity record))
  dbGetEntityAll = runDb . getEntity

  dbGetEntity
    :: (GetEntityField record, PersistRecordBackend record SqlBackend)
    => Key record -> ASPicoM (Maybe (Entity record))
  dbGetEntity key =
    runDb $
    selectFirst [deletedEntityField ==. Nothing, persistIdField ==. key] []

  dbGetEntityBy
    :: PersistRecordBackend record SqlBackend
    => Unique record -> ASPicoM (Maybe (Entity record))
  dbGetEntityBy = runDb . getBy

---------------------------------
-- Additional CREATE Functions --
---------------------------------

-- | Create a new 'Affiliate'.
dbCreateAffiliate
  :: MonadASPicoDb m
  => PartnerId -> AdvertizerId -> ProductId -> m (Entity Affiliate)
dbCreateAffiliate partnerId advId prodId =
  dbCreate $ \currTime ->
    pure $ Affiliate partnerId advId prodId (CreatedTime currTime)

-- | Create a new 'Conversion'.
dbCreateConversion
  :: (MonadASPicoDb m, MonadASPicoAuth m)
  => Key Affiliate -> ConversionId -> m (Entity CompanyUser)
dbCreateConversion affId convId =
  dbCreate $ \currTime ->
    pure $ Conversion affId convId (CreatedTime currTime)

------------------------------
-- Additional GET Functions --
------------------------------

-- | Just like 'dbGetEntitiesBy' with no 'Filter's.
dbGetEntities
  :: ( GetEntityField record
     , MonadASPicoDb m
     , PersistRecordBackend record SqlBackend
     )
  => m [Entity record]
dbGetEntities = dbGetEntitiesBy []

-- | Get all 'Affiliates's that are not deleted.
dbGetAffiliates
  :: MonadASPicoDb m
  => m [Entity Affiliate]
dbGetCompanies = dbGetEntities

-- | Get all 'Conversion's that are not deleted.
dbGetConversions
  :: MonadASPicoDb m
  => m [Entity Conversion]
dbGetCompanyUser = dbGetEntities

dbGetConversionsByAffiliate
  :: MonadASPicoDb m
  => Key Affiliate
  -> m [Entity Conversion]
dbGetConversionsByAffiliate affId =
  dbGetEntitiesBy [ ConversionAffiliate ==. affId ]

----------------------------
-- Higher Level Functions --
----------------------------

-- This should be available in persistent>2.6.
getEntity
  :: (MonadIO m, PersistStoreRead backend, PersistRecordBackend a backend)
  => Key a -> ReaderT backend m (Maybe (Entity a))
getEntity k = fmap (Entity k) <$> get k
