module ASPico.Handler.Root.Track
  ( ApiTrack
  , serverTrack
  ) where

import ASPico.Prelude

import Database.Persist.Sql (Entity(..), Key(..), fromSqlKey)
import Servant ((:>), Capture, Header, Headers, JSON, NoContent(..), ServerT, StdMethod(GET), Verb, addHeader)
import Web.Envelope (Err(..))
import Web.HttpApiData (FromHttpApiData, ToHttpApiData)

import ASPico.Config (Config)
import ASPico.Db (Affiliate(..), URL)
import ASPico.Error (AppErr, AppErrEnum(CouldNotFindAffiliate))
import ASPico.Handler.Consts (affiliateCookie)
import ASPico.Monad (MonadASPicoDb, dbGetEntity)
import ASPico.Servant (Cookie, setCookieHeader)

type ApiTrack = "track"
  :> Capture "aff-id" (Key Affiliate)

  :> Verb 'GET 302 '[JSON]
    (Headers
      '[Header "Location" Location, Header "Set-Cookie" Cookie]
      NoContent)

serverTrack
  :: (MonadError AppErr m, MonadASPicoDb m, MonadReader Config m, MonadIO m)
  => ServerT ApiTrack m
serverTrack = record

record
  :: (MonadError AppErr m, MonadASPicoDb m, MonadReader Config m, MonadIO m)
  => Key Affiliate
  -> m (Headers '[Header "Location" Location, Header "Set-Cookie" Cookie] NoContent)
record k = do
  mEntity <- dbGetEntity k
  case mEntity of
    Nothing -> throwError
      (Err CouldNotFindAffiliate
        (Just $ "Affiliate ID " <> (tshow . fromSqlKey) k <> " doesn't exist")
      )
    Just (Entity _ Affiliate{affiliateRedirectTo}) -> do
      header <- setCookieHeader affiliateCookie (encodeUtf8 . tshow . fromSqlKey $ k) NoContent
      pure $ addHeader (Location affiliateRedirectTo) header

newtype Location =
  Location URL
  deriving ( Data
           , Eq
           , FromHttpApiData
           , Generic
           , IsString
           , Ord
           , Show
           , ToHttpApiData
           , Typeable
           )
