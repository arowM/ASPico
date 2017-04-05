module ASPico.Monad.Db where

import ASPico.Prelude hiding (product)

import Database.Persist.Sql
       (Filter, PersistEntity(..), PersistRecordBackend, PersistStoreRead,
        SqlBackend, (==.), get, getBy, insertEntity, selectFirst,
        selectList)

import ASPico.Db
       (Affiliate(..), Conversion(..), CreatedTime(..), CvId,
        Entity(Entity), EntityField(..), Key, Push(..),
        Unique(UniqueAffiliate), runDb, runDbCurrTime)
import ASPico.Form (AffiliateForm(..), RegisterPushForm(..))
import ASPico.Monad.Base (ASPicoM)

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
    :: (PersistRecordBackend record SqlBackend)
    => [Filter record] -> m [Entity record]

  -- | Just like Persistent's 'getEntity'. Does not filter out deleted records.
  dbGetEntityAll
    :: PersistRecordBackend record SqlBackend
    => Key record -> m (Maybe (Entity record))

  -- | Similar to Persistent's 'getEntity'. Filters out deleted records
  dbGetEntity
    :: (PersistRecordBackend record SqlBackend)
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
    :: (PersistRecordBackend record SqlBackend)
    => [Filter record] -> ASPicoM [Entity record]
  dbGetEntitiesBy filters =
    runDb $ selectList filters []

  dbGetEntityAll
    :: PersistRecordBackend record SqlBackend
    => Key record -> ASPicoM (Maybe (Entity record))
  dbGetEntityAll = runDb . getEntity

  dbGetEntity
    :: (PersistRecordBackend record SqlBackend)
    => Key record -> ASPicoM (Maybe (Entity record))
  dbGetEntity key =
    runDb $
    selectFirst [persistIdField ==. key] []

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
  => AffiliateForm -> m (Entity Affiliate)
dbCreateAffiliate AffiliateForm {..} = do
  mEntity <- dbGetEntityBy $ UniqueAffiliate partner advertizer product redirectTo
  case mEntity of
    Just a -> pure a
    Nothing ->
      dbCreate $ \currTime ->
        pure $
        Affiliate partner advertizer product redirectTo (CreatedTime currTime)

-- | Create a new 'Conversion'.
dbCreateConversion
  :: (MonadASPicoDb m)
  => Key Affiliate -> Maybe CvId -> m (Entity Conversion)
dbCreateConversion affId convId =
  dbCreate $ \currTime ->
    pure $ Conversion affId convId (CreatedTime currTime)

-- | Create a new 'Push'.
dbCreatePush
  :: MonadASPicoDb m
  => RegisterPushForm -> m (Entity Push)
dbCreatePush RegisterPushForm {..} =
  dbCreate $ \currTime ->
    pure $
    Push url (CreatedTime currTime)

------------------------------
-- Additional GET Functions --
------------------------------

-- | Just like 'dbGetEntitiesBy' with no 'Filter's.
dbGetEntities
  :: ( MonadASPicoDb m
     , PersistRecordBackend record SqlBackend
     )
  => m [Entity record]
dbGetEntities = dbGetEntitiesBy []

-- | Get all 'Affiliates's that are not deleted.
dbGetAffiliates
  :: MonadASPicoDb m
  => m [Entity Affiliate]
dbGetAffiliates = dbGetEntities

-- | Get all 'Conversion's that are not deleted.
dbGetConversions
  :: MonadASPicoDb m
  => m [Entity Conversion]
dbGetConversions = dbGetEntities

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
