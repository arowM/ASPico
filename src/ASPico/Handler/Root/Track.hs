module ASPico.Handler.Root.Track
  ( ApiTrack
  , serverTrack
  ) where

import ASPico.Prelude

import Servant ((:>), Capture, Get, Header, Headers, JSON, ServerT)

import ASPico.Db (AffiliateId)
import ASPico.Error (AppErr)
import ASPico.Servant (Cookie, setCookieHeader)
-- import ASPico.Monad (MonadASPicoDb)

type ApiTrack = "track"
  :> Capture "aff-id" AffiliateId
  :> Get '[JSON] (Headers '[Header "Set-Cookie" Cookie] ())

serverTrack
  -- :: (MonadError AppErr m, MonadASPicoDb m)
  :: (MonadError AppErr m)
  => ServerT ApiTrack m
serverTrack = record

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
