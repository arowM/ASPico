module ASPico.Handler.Root.Affiliate
  ( ApiAffiliate
  , serverAffiliate
  ) where

import ASPico.Prelude hiding (product)

import Database.Persist.Sql (Entity(..), fromSqlKey)
import Servant ((:>), FormUrlEncoded, JSON, Post, ReqBody, ServerT)

import ASPico.Envelope (Envelope, returnSuccess)
import ASPico.Error (AppErr)
import ASPico.Form (AffiliateForm(..), AffiliateResp(..))
import ASPico.Monad (MonadASPicoDb, dbCreateAffiliate)

type ApiAffiliate = "affiliate" :> ReqBody '[JSON, FormUrlEncoded] AffiliateForm :> Post '[JSON, FormUrlEncoded] (Envelope AffiliateResp)

serverAffiliate
  :: (MonadError AppErr m, MonadASPicoDb m)
  => ServerT ApiAffiliate m
serverAffiliate = affUrl

affUrl
  :: (MonadError AppErr m, MonadASPicoDb m)
  => AffiliateForm -> m (Envelope AffiliateResp)
affUrl form = do
  Entity k _ <- dbCreateAffiliate form
  returnSuccess . AffiliateResp . tshow . fromSqlKey $ k
