{-# LANGUAGE TemplateHaskell #-}

{-|
Module      : ASPico.Session
Description : session and cookies

This module defines functions and datatypes for working with cookie, in
particular Session cookies.
-}
module ASPico.Session
  ( -- * Session key type class
    HasSessionKey(getSessionKey)
    -- * Session value
  , AffiliateSession(..)
    -- * Decoding Cookie types/functions
  , DecodeCookieError(..)
  , decodeCookie
  )where

import ASPico.Prelude

import Control.FromSum (fromEitherOrM, fromMaybeOrM)
import Data.Aeson (FromJSON, eitherDecodeStrict)
import Data.Aeson.TH (deriveJSON, defaultOptions)
import Network.Wai (Request, requestHeaders)
import Web.ClientSession (decrypt)
import qualified Web.ClientSession as ClientSession
import Web.Cookie (parseCookies)

import ASPico.Db (Affiliate, Key)

class HasSessionKey r where
  getSessionKey :: r -> ClientSession.Key

-- | This has to be a separate type from Session because of the type-level
-- magic that servant is playing with authentication stuff.
newtype AffiliateSession = AffiliateSession
  { unAffiliateSession :: Key Affiliate
  } deriving (Show)

deriveJSON defaultOptions ''AffiliateSession

-- | This type represents the result of trying to pull a cookie out of the
-- 'Request', decrypt it, and parse it as JSON.
data DecodeCookieError
  = CookieDoesNotExist
  -- ^ A cookie with this name does not exist in the 'Request'.
  | CookieCannotBeDecrypted
  -- ^ A cookie with this name exists, but its value cannot be decrypted with
  -- the given 'ClientSession.Key'.
  | CookieCannotBeJsonDecoded Text
  -- ^ A cookie with this name exists and it can be decrypted, but the
  -- resulting value cannot be JSON-decoded to the correct type.

-- | Decode a cookie from a Wai 'Request'.
--
-- In @'decodeCookie' key cookieName request@, decrypt the cookie named
-- @cookieName@ from the Wai 'Request' @request@ with the 'ClientSession.Key'
-- @key@.  Return a 'DecodeCookieError' if there was an error in this process.
decodeCookie
  :: forall m a.
     (FromJSON a, MonadError DecodeCookieError m)
  => ClientSession.Key
  -- ^ 'ClientSession.Key' that is used to decrypt the cookie.
  -> ByteString
  -- ^ Name of the cookie.
  -> Request
  -- ^ Wai 'Request'.
  -> m a
decodeCookie sessionKey cookieName request = do
  encryptedCookieVal <-
    fromMaybeOrM (cookieFromRequest request cookieName) $
    throwError CookieDoesNotExist
  decryptedCookieVal <-
    fromMaybeOrM (decrypt sessionKey encryptedCookieVal) $
    throwError CookieCannotBeDecrypted
  jsonDecodedCookieVal <-
    fromEitherOrM (eitherDecodeStrict decryptedCookieVal) $ \err ->
      throwError (CookieCannotBeJsonDecoded (pack err))
  pure jsonDecodedCookieVal

-- | Takes a cookie name and a wai request as input.  Checks to make sure that
-- there is actually a cookie of that name, and returns it's value as a
-- 'Maybe' 'ByteString'.
cookieFromRequest
  :: Request -- ^ Wai 'Request'
  -> ByteString -- ^ Cookie name
  -> Maybe ByteString  -- ^ Possible Cookie value
cookieFromRequest request cookieName =
  lookup cookieName . parseCookies =<< lookup "Cookie" (requestHeaders request)
