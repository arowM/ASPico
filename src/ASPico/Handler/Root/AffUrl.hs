module ASPico.Handler.Root.AffUrl
  ( ApiAffUrl
  , serverAffUrl
  ) where

import ASPico.Prelude hiding (product)

import Database.Persist.Sql (Entity(..), fromSqlKey)
import Servant ((:>), FormUrlEncoded, JSON, Post, ReqBody, ServerT)

import ASPico.Envelope (Envelope, returnSuccess)
import ASPico.Error (AppErr)
import ASPico.Form (AffUrlForm(..), AffUrlResp(..))
import ASPico.Monad (MonadASPicoDb, dbCreateAffiliate)

type ApiAffUrl = "affiliate_url" :> ReqBody '[JSON, FormUrlEncoded] AffUrlForm :> Post '[JSON, FormUrlEncoded] (Envelope AffUrlResp)

serverAffUrl
  :: (MonadError AppErr m, MonadASPicoDb m)
  => ServerT ApiAffUrl m
serverAffUrl = affUrl

affUrl
  :: (MonadError AppErr m, MonadASPicoDb m)
  => AffUrlForm -> m (Envelope AffUrlResp)
affUrl AffUrlForm{..} = do
  Entity k _ <- dbCreateAffiliate partner advertizer product
  returnSuccess . AffUrlResp . tshow . fromSqlKey $ k
