module ASPico.Db.Pool where

import ASPico.Prelude

import Control.Monad.Logger (runStdoutLoggingT)
import Data.Pool (createPool)
import Data.Time.Clock (NominalDiffTime)
import Database.Persist.Postgresql (openSimpleConn)
import Database.Persist.Sql
       (ConnectionPool, SqlBackend, askLogFunc, close')
import Database.PostgreSQL.Simple (ConnectInfo(..), connect)

-- | Class for things that have a persistent 'ConnectionPool'.
class HasDbPool a where
  getDbPool :: a -> ConnectionPool

type DbPoolConnNum = Int

type DbPoolConnTimeout = NominalDiffTime

type DbHost = String

type DbPort = Word16

type DbUser = String

type DbPass = String

type DbDatabase = String

makePool
  :: forall m.
     (MonadBaseControl IO m, MonadIO m)
  => DbPoolConnNum
  -> DbPoolConnTimeout
  -> DbHost
  -> DbPort
  -> DbUser
  -> DbPass
  -> DbDatabase
  -> m ConnectionPool
makePool dbConnNum dbConnTimeout dbHost dbPort dbUser dbPass dbDb = do
  liftIO $ createPool getConn close' 1 dbConnTimeout dbConnNum
  where
    getConn :: IO SqlBackend
    getConn = do
      let connInfo = createConnInfo dbHost dbPort dbUser dbPass dbDb
      logFunc <- runStdoutLoggingT askLogFunc
      conn <- connect connInfo
      openSimpleConn logFunc conn

createConnInfo :: DbHost
               -> DbPort
               -> DbUser
               -> DbPass
               -> DbDatabase
               -> ConnectInfo
createConnInfo dbHost dbPort dbUser dbPass dbDb =
  ConnectInfo
  { connectHost = dbHost
  , connectPort = dbPort
  , connectUser = dbUser
  , connectPassword = dbPass
  , connectDatabase = dbDb
  }
