module ASPico.Handler.Root.Record
  ( ApiRecord
  , serverRecord
  ) where

import ASPico.Prelude

import Servant ((:>), Capture, Get, Header, Headers, JSON, ServerT)

import ASPico.Db (AffiliateId)
import ASPico.Error (AppErr)
import ASPico.Servant (Cookie, setCookieHeader)
-- import ASPico.Monad (MonadASPicoDb)

type ApiRecord = "record"
  :> Capture "aff-id" AffiliateId
  :> Get '[JSON] (Headers '[Header "Set-Cookie" Cookie] ())

serverRecord
  -- :: (MonadError AppErr m, MonadASPicoDb m)
  :: (MonadError AppErr m)
  => ServerT ApiRecord m
serverRecord = record

cookieName :: ByteString
cookieName = "aspico-aff"

record
  -- :: (MonadError AppErr m, MonadASPicoDb m)
  :: (MonadError AppErr m)
  => AffiliateId
  -> m (Headers '[Header "Set-Cookie" Cookie] ())
record _ =
  setCookieHeader
    cookieName
    "test"
    ()
