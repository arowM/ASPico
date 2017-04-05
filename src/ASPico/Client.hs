{- |
Module      :  ASPico.Client

Define functions to call external APIs.
-}
module ASPico.Client where

import ASPico.Prelude

import Network.HTTP.Client (newManager, defaultManagerSettings)
import Servant.API
import Servant.Client

import ASPico.Db (Conversion)

type API
  = ReqBody '[JSON] Conversion :> Post '[JSON] ()

api :: Proxy API
api = Proxy

push :: Conversion -> ClientM ()
push = client api

runPush :: BaseUrl -> Conversion -> IO ()
runPush url conv = do
  manager <- newManager defaultManagerSettings
  void $ runClientM (push conv) (ClientEnv manager url)
