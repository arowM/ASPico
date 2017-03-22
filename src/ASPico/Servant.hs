{- |
Module      :  ASPico.Servant

Helper functions for working with Servant.
-}

module ASPico.Servant
  ( -- * Data types and functions dealing with cookies
    Cookie
  , setCookieHeader
    -- * Content-type for sending javascript files
  , Png
  ) where

import ASPico.Prelude

import Servant (Accept(..), Header, Headers, MimeRender(..), addHeader)
import Network.HTTP.Media (MediaType, (//))

import ASPico.Http (Cookie, IsCookieSecure(..), createCookie)

-- | In @'addSessionHeader' cookieName cookieValue responseVal@, add the cookie
-- named @cookieName@ and value @cookieValue@ to the response @Set-Cookie@
-- HTTP header around the value @responseVal@.
setCookieHeader
  :: (Monad m)
  => ByteString
  -> ByteString
  -> a
  -> m (Headers '[Header "Set-Cookie" Cookie] a)
setCookieHeader cookieName cookieVal responseVal = do
  let
    -- TODO
    isCookieSecure = CookieInsecure
    cookie = createCookie cookieName cookieVal isCookieSecure
  return $ addHeader cookie responseVal

-- | Png content-type for Servant.
data Png deriving Typeable

instance Accept Png where
  contentType :: Proxy Png -> MediaType
  contentType _ = "image" // "png"

instance MimeRender Png LText where
  mimeRender :: Proxy Png -> LText -> LByteString
  mimeRender _ = encodeUtf8

instance MimeRender Png Text where
  mimeRender :: Proxy Png -> Text -> LByteString
  mimeRender _ = encodeUtf8 . fromStrict

instance MimeRender Png LByteString where
  mimeRender :: Proxy Png -> LByteString -> LByteString
  mimeRender _ = id

instance MimeRender Png ByteString where
  mimeRender :: Proxy Png -> ByteString -> LByteString
  mimeRender _ = fromStrict
