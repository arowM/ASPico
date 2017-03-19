module ASPico.Monad.Http
  ( module ASPico.Monad.Http
  , HttpError(..)
  ) where

import ASPico.Prelude

import Network.HTTP.Simple (getResponseBody, setRequestHeader)
import Text.HTML.DOM (parseLBS)
import Text.XML (Document)

import ASPico.Http (HttpError(..), httpFetch, parseReq)
import ASPico.Monad.Base (ASPicoM)

class Monad m =>
      MonadASPicoHttp m where
  httpGetUrl :: Text -> m (Either HttpError Document)

instance MonadASPicoHttp ASPicoM where
  httpGetUrl :: Text -> ASPicoM (Either HttpError Document)
  httpGetUrl url =
    runExceptT $ do
      -- TODO: There is an error in the hs-tls library which prevents tls
      -- connections to certain domains.
      -- Here is the bug describing this error:
      -- https://github.com/vincenthz/hs-tls/issues/152
      request <- parseReq url
      let newRequest =
            setRequestHeader
              "User-Agent"
              [ "Mozilla/5.0 (X11; Linux x86_64; rv:49.0) Gecko/20100101 Firefox/49.0"
              ] $
            setRequestHeader
              "Accept"
              ["text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8"] $
            setRequestHeader "AcceptLanguage" ["en-US,en;q=0.5,ja;q=0.5"] $
            setRequestHeader "Accept-Encoding" ["gzip, deflate"] $
            setRequestHeader "DNT" ["1"] $
            setRequestHeader "Connection" ["keep-alive"] $
            setRequestHeader "Upgrade-Insecure-Requests" ["1"] $
            setRequestHeader "Cache-Control" ["max-age=0"] request
      response <- getResponseBody <$> httpFetch newRequest
      let doc = parseLBS response
      pure doc
