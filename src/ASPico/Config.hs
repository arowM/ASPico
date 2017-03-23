module ASPico.Config where

import ASPico.Prelude

import Control.FromSum (fromEitherOr)
import Data.ByteString.Base64 (decode)
import Database.Persist.Postgresql (ConnectionPool)
import Network.HTTP.Client
       (HasHttpManager(getHttpManager), Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.Wai.Handler.Warp (Port)
import Network.Wai.Middleware.RequestLogger
       (logStdoutDev, logStdout)
import Network.Wai (Middleware)
import System.ReadEnvVar (lookupEnvDef, readEnvVarDef)
import Web.ClientSession (Key, initKey)

import ASPico.Db
       (DbDatabase, DbHost, DbPass, DbPoolConnNum, DbPoolConnTimeout,
        DbPort, DbUser, HasDbPool(..), makePool)
import ASPico.Environment (Environment(..), HasEnv(..))
import ASPico.Host (HasHost(..), HasProtocol(..))

data Config = Config
  { configEnv :: !Environment
  , configHost :: !Text
  , configHttpManager :: !Manager
  , configPool :: !ConnectionPool
  , configPort :: !Port
  , configProtocol :: !Text
  , configSessionKey :: !Key
  }

instance HasDbPool Config where
  getDbPool :: Config -> ConnectionPool
  getDbPool = configPool

instance HasEnv Config where
  getEnv :: Config -> Environment
  getEnv = configEnv

instance HasHost Config where
  getHost :: Config -> Text
  getHost = configHost

instance HasHttpManager Config where
  getHttpManager :: Config -> Manager
  getHttpManager = configHttpManager

instance HasProtocol Config where
  getProtocol :: Config -> Text
  getProtocol = configProtocol

getRequestLoggerMiddleware :: Environment -> Middleware
getRequestLoggerMiddleware Test = id
getRequestLoggerMiddleware Development = logStdoutDev
getRequestLoggerMiddleware Production = logStdout

type Host = String

type Protocol = String

type Base64SessionKey = ByteString

createConfig
  :: (MonadBaseControl IO m, MonadIO m)
  => Environment
  -> Port
  -> DbPoolConnNum
  -> DbPoolConnTimeout
  -> DbHost
  -> DbPort
  -> DbUser
  -> DbPass
  -> DbDatabase
  -> Host
  -> Protocol
  -> Base64SessionKey
  -> m Config
createConfig env port dbConnNum dbConnTimeout dbHost dbPort dbUser dbPass dbDB host protocol base64SessionKey = do
  httpManager <- liftIO $ newManager tlsManagerSettings
  pool <- makePool dbConnNum dbConnTimeout dbHost dbPort dbUser dbPass dbDB
  let eitherSessionKey = initKey =<< decode base64SessionKey
      sessionKey =
        fromEitherOr eitherSessionKey $ \err ->
          error $ "Failed to decode the session key: " <> err
  pure
    Config
    { configEnv = env
    , configHost = pack host
    , configHttpManager = httpManager
    , configPool = pool
    , configPort = port
    , configProtocol = pack protocol
    , configSessionKey = sessionKey
    }

createConfigFromEnvVars :: IO Config
createConfigFromEnvVars = do
  port <- readEnvVarDef "PORT" 8081
  env <- readEnvVarDef "ASPICO_ENV" Development
  dbConnNum <- readEnvVarDef "ASPICO_DB_CONN_NUM" 10
  dbConnTimeout <-
    fromInteger <$> readEnvVarDef "ASPICO_DB_CONN_TIMEOUT" 60 -- 1 minute
  dbHost <- lookupEnvDef "ASPICO_DB_HOST" "localhost"
  dbPort <- readEnvVarDef "ASPICO_DB_PORT" 5432
  dbUser <- lookupEnvDef "ASPICO_DB_USER" "aspico"
  dbPass <- lookupEnvDef "ASPICO_DB_PASSWORD" "3pUiRmS2Rv6f28uW"
  dbDatabase <- lookupEnvDef "ASPICO_DB_DATABASE" "aspico"
  host <- lookupEnvDef "ASPICO_HOST" "localhost:8081"
  protocol <- lookupEnvDef "ASPICO_PROTOCOL" "http"
  -- A new session key can be created in GHCI with the command
  -- ASPico.Dev.createSessionKey.
  base64SessionKey <-
    lookupEnvDef
      "ASPICO_BASE64_SESSION_KEY"
      "hDwxYklK+fMs1p2ycvGGh/nQb5FDJfKYI+Msk8IreM7+NWkh8nA/Kezw+xj/FnifLHvr5nXuDQV2qfQ70oKBicYYiWB+Y/lzIIARtG1ZtyEFMw1HyaUeARjG4Ue+U3kX"
  createConfig
    env
    port
    dbConnNum
    dbConnTimeout
    dbHost
    dbPort
    dbUser
    dbPass
    dbDatabase
    host
    protocol
    base64SessionKey
