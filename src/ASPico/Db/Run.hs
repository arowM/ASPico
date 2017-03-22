
module ASPico.Db.Run where

import ASPico.Prelude

import Database.Persist.Postgresql ( SqlBackend(..), runMigration, runSqlPool )

import ASPico.Db.Pool ( HasDbPool(..) )
import ASPico.Db.Models ( migrateAll )

-- | Run all sql migration.
doMigrations :: (MonadIO m) => ReaderT SqlBackend m ()
doMigrations = runMigration migrateAll

-- | Run a Persistent query.
runDb :: ( MonadIO m
         , MonadReader r m
         , HasDbPool r
         , MonadBaseControl IO m
         )
      => ReaderT SqlBackend m a
      -> m a
runDb query = reader getDbPool >>= runSqlPool query

-- | Just like 'runDb' but provide the current time to the callback.
runDbCurrTime :: ( MonadIO m
                 , MonadReader r m
                 , HasDbPool r
                 , MonadBaseControl IO m
                 )
              => (UTCTime -> ReaderT SqlBackend m b)
              -> m b
runDbCurrTime query = do
    currentTime <- liftIO getCurrentTime
    runDb $ query currentTime
