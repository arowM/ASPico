{-# LANGUAGE TemplateHaskell #-}

module ASPico.Handler.Root.Conversion
  ( ApiConversion
  , serverConversion
  ) where

import ASPico.Prelude

import Database.Persist.Sql (Entity(..))
import Servant ((:>), Get, Header(..), QueryParam, ServerT)

import ASPico.Client (runPush)
import ASPico.Config (Config(..))
import ASPico.Db (Affiliate(..), Conversion(..), CvId)
import ASPico.Error (AppErr)
import ASPico.Form (RunPushForm(..))
import ASPico.Handler.Root.Conversion.TH (pngContent)
import ASPico.Handler.Cookie (AffiliateCookie(..))
import ASPico.Monad
       (MonadASPicoDb, dbCreateConversion, dbGetEntity)
import ASPico.Servant (Png)

type ApiConversion = "cv"
  :> QueryParam "cid" CvId
  :> Header "Cookie" AffiliateCookie
  :> Get '[Png] ByteString

serverConversion
  :: (MonadError AppErr m, MonadASPicoDb m, MonadReader Config m, MonadIO m)
  => ServerT ApiConversion m
serverConversion = conversionHandler

conversionHandler
  :: (MonadError AppErr m, MonadASPicoDb m, MonadReader Config m, MonadIO m)
  => Maybe CvId -> Maybe AffiliateCookie -> m ByteString
conversionHandler mconvId mcookie = do
  case mcookie of
    Just (AffiliateCookie affId) -> do
      (Entity _ Conversion{..}) <- dbCreateConversion affId mconvId
      mAffiliate <- dbGetEntity conversionAffiliate
      mUrl <- asks configPushUrl
      case (entityVal <$> mAffiliate, mUrl) of
        (Just Affiliate{..}, Just url) -> liftIO . runPush url $ RunPushForm
          { partner = affiliatePartner
          , advertizer = affiliateAdvertizer
          , product = affiliateProduct
          , conversion = conversionConversion
          , created = conversionCreated
          }
        _ -> pure ()
    _ -> pure ()
  pure $(pngContent)
