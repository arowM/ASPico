module ASPico.Handler.Root.Track
  ( ApiTrack
  , serverTrack
  ) where

import ASPico.Prelude

import Database.Persist.Sql (Entity(..), Key(..), fromSqlKey)
import Servant ((:>), (:<|>)((:<|>)), Capture, Header, Headers, JSON, NoContent(..), ServerT, StdMethod(GET), Verb, addHeader)
import Web.Envelope (Err(..))
import Web.HttpApiData (FromHttpApiData, ToHttpApiData)

import ASPico.Config (Config)
import ASPico.Db (Affiliate(..), URL)
import ASPico.Error (AppErr, AppErrEnum(CouldNotFindAffiliate))
import ASPico.Handler.Consts (affiliateCookie)
import ASPico.Handler.Cookie (AffiliateCookie(..))
import ASPico.Monad (MonadASPicoDb, dbGetEntity)
import ASPico.Servant (Cookie, setCookieHeader)

type ApiTrack = "track"
  :> (
    Capture "aff-id" (Key Affiliate)
    :> Header "Cookie" AffiliateCookie
    :> Verb 'GET 302 '[JSON]
      (Headers
        '[Header "Location" Location, Header "Set-Cookie" Cookie]
        NoContent)
    :<|>
    Capture "aff-id" (Key Affiliate)
    :> "force"
    :> Verb 'GET 302 '[JSON]
      (Headers
        '[Header "Location" Location, Header "Set-Cookie" Cookie]
        NoContent)
  )

serverTrack
  :: (MonadError AppErr m, MonadASPicoDb m, MonadReader Config m, MonadIO m)
  => ServerT ApiTrack m
serverTrack = record :<|> recordForce

record
  :: (MonadError AppErr m, MonadASPicoDb m, MonadReader Config m, MonadIO m)
  => Key Affiliate
  -> Maybe AffiliateCookie
  -> m (Headers '[Header "Location" Location, Header "Set-Cookie" Cookie] NoContent)
record k mcookie = do
  mEntity <- dbGetEntity k
  case (mEntity, mcookie) of
    (Nothing, _) ->
      throwError
        (Err
           CouldNotFindAffiliate
           (Just $ "Affiliate ID " <> (tshow . fromSqlKey) k <> " doesn't exist"))
    (Just (Entity _ Affiliate {affiliateRedirectTo}), Just (AffiliateCookie affId)) -> do
      setAffiliateCookie affId affiliateRedirectTo
    (Just (Entity _ Affiliate {affiliateRedirectTo}), Nothing) -> do
      setAffiliateCookie k affiliateRedirectTo


recordForce
  :: (MonadError AppErr m, MonadASPicoDb m, MonadReader Config m, MonadIO m)
  => Key Affiliate
  -> m (Headers '[Header "Location" Location, Header "Set-Cookie" Cookie] NoContent)
recordForce k = do
  mEntity <- dbGetEntity k
  case mEntity of
    Nothing ->
      throwError
        (Err
           CouldNotFindAffiliate
           (Just $ "Affiliate ID " <> (tshow . fromSqlKey) k <> " doesn't exist"))
    (Just (Entity _ Affiliate {affiliateRedirectTo})) -> do
      setAffiliateCookie k affiliateRedirectTo


setAffiliateCookie
  :: (MonadError AppErr m, MonadASPicoDb m, MonadReader Config m, MonadIO m)
  => Key Affiliate
  -> URL
  -> m (Headers '[Header "Location" Location, Header "Set-Cookie" Cookie] NoContent)
setAffiliateCookie k url = do
  header <-
    setCookieHeader
      affiliateCookie
      (encodeUtf8 . tshow . fromSqlKey $ k)
      NoContent
  pure $ addHeader (Location url) header


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
