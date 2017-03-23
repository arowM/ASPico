module ASPico.Handler.Root where

import ASPico.Prelude

import Servant (ServerT, (:<|>)((:<|>)))

import ASPico.Error (AppErr)
import ASPico.Handler.Root.AffUrl (ApiAffUrl, serverAffUrl)
import ASPico.Handler.Root.Conversion (ApiConversion, serverConversion)
import ASPico.Handler.Root.Record (ApiRecord, serverRecord)
import ASPico.Monad (MonadASPico)

type ApiRoot = ApiAffUrl :<|> ApiConversion :<|> ApiRecord

serverRoot
  :: (MonadError AppErr m, MonadASPico m)
  => ServerT ApiRoot m
serverRoot = serverAffUrl :<|> serverConversion :<|> serverRecord
