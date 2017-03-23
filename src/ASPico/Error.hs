{-|
Description : Errors used by the web api.

This module contains errors used by the web api and methods to convert them to
a ServantErr.
-}

module ASPico.Error where

import ASPico.Prelude

import Data.Aeson (ToJSON(..), Value(..), encode)
import Database.PostgreSQL.Simple (SqlError(..))
import Servant (ServantErr(..), err400, err401, err404, err500)
import Web.Envelope (Err(..))
import Web.HttpApiData (ToHttpApiData(toQueryParam))

import ASPico.Environment ( Environment(..), HasEnv(..) )

type AppErr = Err AppErrEnum

instance Exception AppErr

-- | A union of different errors that can occur in our application.
data AppErrEnum
  = AuthCookieDoesNotExist
  -- ^ A cookie with this name does not exist in the Wai 'Request'.
  | AuthCookieCannotBeDecrypted
  -- ^ A cookie with the correct name exists in the Wai 'Request', but its
  -- value cannot be decrypted with the server's 'ClientSession.Key'.
  | AuthCookieCannotBeJsonDecoded
  -- ^ A cookie with the correct name exists in the Wai 'Request', and it can be
  -- decrypted with the server's 'ClientSession.Key', but it fails JSON
  -- decoding.
  | AuthDbPasswordCheckFail
  -- ^ The password check failed for a 'CompanyUser', or the 'CompanyUser'
  -- does not exist in the database.
  | CouldNotFindAffiliate
  -- ^ When a conversion fires,
  -- we weren't able to find the `Affiliate` in the database.
  | OtherException
  -- ^ A wrapper for any other type of exception not covered by other errors.
  | OtherSqlException
  -- ^ A wrapper for any type of sql exception.
  deriving (Data, Eq, Generic, Show, Typeable)

instance ToJSON AppErrEnum where
  toJSON :: AppErrEnum -> Value
  toJSON = String . tshow

instance ToHttpApiData AppErrEnum where
  toQueryParam :: AppErrEnum -> Text
  toQueryParam = tshow

-- | Convert 'SomeException' to 'Err'.
someExceptionToAppErr
  :: HasEnv r
  => r -> SomeException -> AppErr
someExceptionToAppErr envContainer (SomeException e) =
  let env = getEnv envContainer
      extra =
        case env of
          Development ->
            let typ = typeOf e
            in Just $
               "SomeException occured. exception type: " <> tshow typ <>
               ", exception: " <>
               tshow e
          _ -> Just "SomeException occured"
  in Err {errErr = OtherException, errExtra = extra}

sqlErrorToAppErr
  :: HasEnv r
  => r -> SqlError -> AppErr
sqlErrorToAppErr envContainer sqlError =
  let env = getEnv envContainer
  in Err
     { errErr = OtherSqlException
     , errExtra =
         case env of
           Development -> Just $ tshow sqlError
           _ -> Nothing
     }

-- | Converts an 'Err' to a 'ServantErr' so we can return it from our api.
appErrToServantErr :: AppErr -> ServantErr
appErrToServantErr err =
  case errErr err of
    AuthCookieDoesNotExist -> setBody err401
    AuthCookieCannotBeDecrypted -> setBody err401
    AuthCookieCannotBeJsonDecoded -> setBody err401
    AuthDbPasswordCheckFail -> setBody err400
    CouldNotFindAffiliate -> setBody err404
    OtherException -> setBody err500
    OtherSqlException -> setBody err500
  where
    setBody :: ServantErr -> ServantErr
    setBody servantErr = servantErr {errBody = encode err}

-- | Convert a 'SomeException' to a 'ServantErr'.
someExceptionToServantErr
  :: HasEnv r
  => r -> SomeException -> ServantErr
someExceptionToServantErr envContainer =
  appErrToServantErr . someExceptionToAppErr envContainer
