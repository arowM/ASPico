module ASPico.Handler.Root.AffUrl
  ( ApiAffUrl
  , serverAffUrl
  ) where

import ASPico.Prelude

import Servant ((:>), FormUrlEncoded, JSON, Post, ReqBody, ServerT)

import ASPico.Envelope (Envelope, returnSuccess)
import ASPico.Error (AppErr)
import ASPico.Form (AffUrlForm, AffUrlResp(..))
-- import ASPico.Monad (MonadASPicoDb, dbCreateAffiliate)

type ApiAffUrl = "affiliate_url" :> ReqBody '[JSON, FormUrlEncoded] AffUrlForm :> Post '[JSON, FormUrlEncoded] (Envelope AffUrlResp)

serverAffUrl
  -- :: (MonadError AppErr m, MonadASPicoDb m)
  :: (MonadError AppErr m)
  => ServerT ApiAffUrl m
serverAffUrl = affUrl

affUrl
  -- :: (MonadError AppErr m, MonadASPicoDb m)
  :: (MonadError AppErr m)
  => AffUrlForm -> m (Envelope AffUrlResp)
affUrl _ = returnSuccess $
  AffUrlResp "test"
