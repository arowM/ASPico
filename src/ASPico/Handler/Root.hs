module ASPico.Handler.Root where

import ASPico.Prelude

import Servant (ServerT, (:<|>)((:<|>)))

import ASPico.Config (Config)
import ASPico.Error (AppErr)
import ASPico.Handler.Root.Affiliate (ApiAffiliate, serverAffiliate)
import ASPico.Handler.Root.Conversion (ApiConversion, serverConversion)
import ASPico.Handler.Root.Track (ApiTrack, serverTrack)
import ASPico.Monad (MonadASPico)

type ApiRoot = ApiAffiliate :<|> ApiConversion :<|> ApiTrack

serverRoot
  :: (MonadError AppErr m, MonadASPico m, MonadReader Config m, MonadIO m)
  => ServerT ApiRoot m
serverRoot =
  serverAffiliate :<|> serverConversion :<|> serverTrack
