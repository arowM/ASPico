{- |
Module      :  ASPico.Http

Helper functions for working with HTTP.  Currently only exports functions
pertaining to cookies.
-}

module ASPico.Http
  ( -- * Cookie-related functionality
    Cookie
  , IsCookieSecure(CookieInsecure, CookieSecure)
  , isCookieSecureToBool
  , createCookie
    -- * Http Request functionality
  , HttpError(..)
  , httpFetch
  , parseReq
  ) where

import ASPico.Prelude

import Blaze.ByteString.Builder (toByteString)
import Control.FromSum (fromEitherM)
import Data.Default (Default(def))
import Network.HTTP.Simple
       (HttpException, Request, Response, httpLBS, parseRequest)
import Web.Cookie
       (SetCookie, renderSetCookie, setCookieHttpOnly, setCookieMaxAge,
        setCookieName, setCookiePath, setCookieSecure, setCookieValue)
import Web.HttpApiData (ToHttpApiData(toHeader, toUrlPiece))

-- | A wrapper around the raw cookie created from a combination of
-- 'createSetCookie' and 'renderSetCookie'.  This is used so that we can easily
-- create an instance of 'ToHttpApiData' for the cookie.  This is used in
-- Servant when setting the cookie as a 'Servant.Header' in "ASPico.Servant".
data Cookie = Cookie { unCookieVal :: ByteString }
  deriving (Data, Eq, Generic, Show, Typeable)

instance ToHttpApiData Cookie where
  toUrlPiece :: Cookie -> Text
  toUrlPiece = decodeUtf8 . toHeader

  toHeader :: Cookie -> ByteString
  toHeader = unCookieVal

-- | Data type representing whether or not this cookie is secure.
-- This option instructs the browser to only send the cookie over HTTPS.
-- See 'setCookieSecure'.
data IsCookieSecure = CookieInsecure | CookieSecure
  deriving (Data, Eq, Generic, Show, Typeable)

-- | Converts 'IsCookieSecure' to a 'Bool'.  'CookieInsecure' is converted to
-- 'False', while 'CookieSecure' is converted to 'True'.
isCookieSecureToBool :: IsCookieSecure -> Bool
isCookieSecureToBool CookieInsecure = False
isCookieSecureToBool CookieSecure = True

-- | Create a cookie from an encrypted 'Session'.  'Session' should be
-- encrypted with 'encryptCookie'.  The 'ByteString' produced from this
-- function is the entire cookie, and should be sent back to the user with the
-- 'addHeader' Servant function.
createCookie :: ByteString -> ByteString -> IsCookieSecure -> Cookie
createCookie cookieName cookieValue isCookieSecure =
    let setCookie = createSetCookie cookieName cookieValue isCookieSecure
    in Cookie . toByteString $ renderSetCookie setCookie

createSetCookie :: ByteString -> ByteString -> IsCookieSecure -> SetCookie
createSetCookie cookieName cookieVal isCookieSecure = def
    { setCookieHttpOnly = True
    , setCookieMaxAge   = Just $ 60 * 60 * 24 * 30 -- one month
    , setCookieName     = cookieName
    , setCookiePath     = Just "/"
    , setCookieSecure   = isCookieSecureToBool isCookieSecure
    , setCookieValue    = cookieVal
    }

-- | Wrap up errors thrown by 'parseReq' and 'httpFetch'.
data HttpError
  = HttpErrorParseUrl HttpException
  -- ^ Error with parsing a URL.
  | HttpErrorFetch HttpException
  -- ^ Error with fetching a URL.
  deriving (Show, Typeable)

-- | Similar to 'parseRequest', but catch the 'HttpException' it throws.
parseReq
  :: forall m.
     (MonadError HttpError m, MonadIO m)
  => Text -> m Request
parseReq = fromEitherM throwError <=< liftIO . parseReq' . unpack
  where
    parseReq' :: String -> IO (Either HttpError Request)
    parseReq' url =
      catch (Right <$> parseRequest url) $ pure . Left . HttpErrorParseUrl

-- | Similar to 'httpLBS', but catch the 'HttpException' it throws.
httpFetch
  :: forall m.
     (MonadError HttpError m, MonadIO m)
  => Request -> m (Response LByteString)
httpFetch = fromEitherM throwError <=< liftIO . httpFetchIO
  where
    httpFetchIO :: Request -> IO (Either HttpError (Response LByteString))
    httpFetchIO request =
      catch (Right <$> httpLBS request) $ pure . Left . HttpErrorFetch
