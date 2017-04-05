module ASPico.Config where

import ASPico.Prelude

import Database.Persist.Postgresql (ConnectionPool)
import Network.HTTP.Client
       (HasHttpManager(getHttpManager), Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.Wai.Handler.Warp (Port)
import Network.Wai.Middleware.RequestLogger
       (logStdoutDev, logStdout)
import Network.Wai (Middleware)
import Servant.Client (BaseUrl, parseBaseUrl)
import System.Environment (lookupEnv)
import System.ReadEnvVar (lookupEnvDef, readEnvVarDef)

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
  , configPushUrl :: !(Maybe BaseUrl)
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
  -> Maybe BaseUrl
  -> m Config
createConfig env port dbConnNum dbConnTimeout dbHost dbPort dbUser dbPass dbDB host protocol pushUrl = do
  httpManager <- liftIO $ newManager tlsManagerSettings
  pool <- makePool dbConnNum dbConnTimeout dbHost dbPort dbUser dbPass dbDB
  pure
    Config
    { configEnv = env
    , configHost = pack host
    , configHttpManager = httpManager
    , configPool = pool
    , configPort = port
    , configProtocol = pack protocol
    , configPushUrl = pushUrl
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
  mpushUrl <- lookupEnv "ASPICO_PUSH_URL"
  pushUrl <- case mpushUrl of
    Nothing -> pure Nothing
    Just str -> pure <$> parseBaseUrl str
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
    pushUrl
