module ASPico.Handler.Root.Conversion
  ( ApiConversion
  , serverConversion
  ) where

import ASPico.Prelude

import Servant ((:>), Get, Header(..), QueryParam, ServerT)

import ASPico.Db (AffiliateId, CvId)
import ASPico.Error (AppErr)
import ASPico.Monad (MonadASPicoDb, dbCreateConversion)
import ASPico.Servant (Png)
import Web.HttpApiData (FromHttpApiData, ToHttpApiData)

type ApiConversion = "cv"
  :> QueryParam "cid" CvId
  :> Header "Cookie" Cookie
  :> Get '[Png] ByteString

newtype Cookie = Cookie AffiliateId
  deriving (Eq, Show, FromHttpApiData, ToHttpApiData)

serverConversion
  :: (MonadError AppErr m, MonadASPicoDb m)
  => ServerT ApiConversion m
serverConversion =
  conversion

conversion
  :: (MonadError AppErr m, MonadASPicoDb m)
  => Maybe CvId
  -> Maybe Cookie
  -> m ByteString
conversion mconvId mcookie = do
  case (mconvId, mcookie) of
    (Just convId, Just (Cookie affId)) ->
      void $ dbCreateConversion
        affId
        convId
    _ -> pure ()
  pure ""
