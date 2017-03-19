module ASPico.Api
  ( defaultMainApi
  ) where

import ASPico.Prelude

import Database.Persist.Sql (runSqlPool)
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp (run)

import ASPico.Config
       (Config(..), createConfigFromEnvVars, getRequestLoggerMiddleware)
import ASPico.Db (doMigrations)
import ASPico.Handler (app)

setup :: IO (Config, Middleware)
setup = do
  cfg <- createConfigFromEnvVars
  let requestLoggerMiddleware = getRequestLoggerMiddleware $ configEnv cfg
  return (cfg, requestLoggerMiddleware)

defaultMainApi :: IO ()
defaultMainApi = do
  (cfg, requestLoggerMiddleware) <- setup
  -- TODO: Probably shouldn't do migrations in production automatically.
  runSqlPool doMigrations $ configPool cfg
  let port = configPort cfg
  putStrLn $ "ASPico running on port " <> tshow port <> "..."
  -- Don't deal with CORS.  Just let nginx deal with it.
  run port . requestLoggerMiddleware $ app cfg
