module ASPico.Handler.Cookie
  ( AffiliateCookie(..)
  ) where

import ASPico.Prelude

import Database.Persist.Sql (Key(..), toSqlKey)
import Web.Cookie (parseCookies)
import Web.HttpApiData (FromHttpApiData(..), ToHttpApiData)

import ASPico.Db (Affiliate(..))
import ASPico.Handler.Consts (affiliateCookie)

-- ----------
--  Cookie
-- ----------
newtype AffiliateCookie =
  AffiliateCookie (Key Affiliate)
  deriving (Eq, Read, Show, ToHttpApiData)

instance FromHttpApiData AffiliateCookie where
  parseUrlPiece txt =
    case readMay . decodeUtf8 =<< maybeVal of
      Nothing -> Left "Could not found affiliate id in cookie."
      Just aff -> Right . AffiliateCookie . toSqlKey $ aff
    where
      maybeVal :: Maybe ByteString
      maybeVal = lookup affiliateCookie . parseCookies . encodeUtf8 $ txt
