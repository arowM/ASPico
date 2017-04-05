{-# LANGUAGE TemplateHaskell #-}

module ASPico.Handler.Root.Conversion
  ( ApiConversion
  , serverConversion
  ) where

import ASPico.Prelude

import Database.Persist.Sql (Entity(..), Key(..), toSqlKey)
import Servant ((:>), Get, Header(..), QueryParam, ServerT)
import Web.Cookie (parseCookies)
import Web.HttpApiData (FromHttpApiData(..), ToHttpApiData)

import ASPico.Client (runPush)
import ASPico.Config (Config(..))
import ASPico.Db (Affiliate, CvId)
import ASPico.Error (AppErr)
import ASPico.Handler.Consts (affiliateCookie)
import ASPico.Handler.Root.Conversion.TH (pngContent)
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
  :: (MonadError AppErr m, MonadASPicoDb m, MonadReader Config m, MonadIO m)
  => ServerT ApiConversion m
serverConversion = conversion

conversion
  :: (MonadError AppErr m, MonadASPicoDb m, MonadReader Config m, MonadIO m)
  => Maybe CvId -> Maybe AffiliateCookie -> m ByteString
conversion mconvId mcookie = do
  case mcookie of
    Just (AffiliateCookie affId) -> do
      (Entity _ conv) <- dbCreateConversion affId mconvId
      mUrl <- asks configPushUrl
      case mUrl of
        Just url -> liftIO $ runPush url conv
        _ -> pure ()
    _ -> pure ()
  pure $(pngContent)
