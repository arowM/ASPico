{- |
Module      :  ASPico.Client

Define functions to call external APIs.
-}
module ASPico.Client where

import ASPico.Prelude

import Network.HTTP.Client (newManager, defaultManagerSettings)
import Servant.API
import Servant.Client

import ASPico.Form (RunPushForm)

type API
  = ReqBody '[JSON] RunPushForm :> Post '[JSON] ()

api :: Proxy API
api = Proxy

push :: RunPushForm -> ClientM ()
push = client api

runPush :: BaseUrl -> RunPushForm -> IO ()
runPush url conv = do
  manager <- newManager defaultManagerSettings
  void $ runClientM (push conv) (ClientEnv manager url)
