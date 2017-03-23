module ASPico.Handler.Root.Conversion
  ( ApiConversion
  , serverConversion
  ) where

import ASPico.Prelude

import Database.Persist.Sql (Key(..), toSqlKey)
import Servant ((:>), Get, Header(..), QueryParam, ServerT)
import Web.Cookie (parseCookies)
import Web.HttpApiData (FromHttpApiData(..), ToHttpApiData)

import ASPico.Db (Affiliate, CvId)
import ASPico.Error (AppErr)
import ASPico.Handler.Consts (affiliateCookie)
import ASPico.Monad (MonadASPicoDb, dbCreateConversion)
import ASPico.Servant (Png)

type ApiConversion = "cv"
  :> QueryParam "cid" CvId
  :> Header "Cookie" AffiliateCookie
  :> Get '[Png] ByteString

newtype AffiliateCookie = AffiliateCookie (Key Affiliate)
  deriving (Eq, Read, Show, ToHttpApiData)

instance FromHttpApiData AffiliateCookie where
  parseUrlPiece txt =
    case readMay . decodeUtf8 =<< maybeVal of
      Nothing -> Left "Could not found affiliate id in cookie."
      Just aff -> Right . AffiliateCookie . toSqlKey $ aff
    where
      maybeVal :: Maybe ByteString
      maybeVal = lookup affiliateCookie . parseCookies . encodeUtf8 $ txt

serverConversion
  :: (MonadError AppErr m, MonadASPicoDb m)
  => ServerT ApiConversion m
serverConversion =
  conversion

conversion
  :: (MonadError AppErr m, MonadASPicoDb m)
  => Maybe CvId
  -> Maybe AffiliateCookie
  -> m ByteString
conversion mconvId mcookie = do
  case (mconvId, mcookie) of
    (Just convId, Just (AffiliateCookie affId)) ->
      void $ dbCreateConversion
        affId
        convId
    _ -> pure ()
  pure ""
