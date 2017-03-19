
module ASPico.Db.Run where

import ASPico.Prelude

import Database.Persist.Postgresql ( SqlBackend(..), runMigration, runSqlPool )
import Database.PostgreSQL.Simple ( SqlError(..) )
import Web.Envelope ( throwEnvelopeErr )

import ASPico.Db.Pool ( HasDbPool(..) )
import ASPico.Error ( AppErr, AppErrEnum(..) )
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

-- | Just like 'runDB' but catch SQL uniqueness violation errors and
-- rethrow them as more specific ASPico errors.
runDbSafe :: forall m r b .  ( MonadError AppErr m
                             , MonadIO m
                             , MonadReader r m
                             , HasDbPool r
                             , MonadBaseControl IO m
                             )
          => ReaderT SqlBackend m b
          -> m b
runDbSafe query = undefined -- do
    -- maybeResult <- try $ runDb query
    -- case maybeResult of
    --     Right result -> return result
    --     Left sqlError@SqlError{..}
    --         | isUniquenessViolation sqlError -> handleUniquenessViolation sqlError
    --         | otherwise -> doError sqlError OtherSqlException "unknown sql error"
  where
    -- | Return True if an 'SqlError' is a uniqueness violation.
    isUniquenessViolation :: SqlError -> Bool
    isUniquenessViolation SqlError{..} =
        "duplicate key value violates unique constraint" `isInfixOf` sqlErrorMsg

    handleUniquenessViolation :: SqlError -> m b
    handleUniquenessViolation sqlError@SqlError{..}
        | "unique_user_email" `isInfixOf` sqlErrorMsg =
            undefined
            -- doError sqlError UserEmailAlreadyExists
            --     "uniqueness error (unique_user_email)"
        | otherwise =
            undefined
            -- doError sqlError SqlErr "unknown uniqueness error"

    doError :: SqlError -> AppErrEnum -> Text -> m b
    doError sqlError errEnum msg = do
        let errMsg = "got " <> msg <> " in runDbSafe: " <> tshow sqlError
        putStrLn errMsg
        throwEnvelopeErr errEnum $ tshow sqlError

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

-- | Combination of 'runDbCurrTime' and 'runDbSafe'.
runDbSafeCurrTime :: ( MonadError AppErr m
                     , MonadIO m
                     , MonadReader r m
                     , HasDbPool r
                     , MonadBaseControl IO m
                     )
                  => (UTCTime -> ReaderT SqlBackend m b)
                  -> m b
runDbSafeCurrTime query = do
    currentTime <- liftIO getCurrentTime
    runDbSafe $ query currentTime
