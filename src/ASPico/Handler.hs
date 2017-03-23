module ASPico.Handler where

import ASPico.Prelude

import Control.FromSum (fromEitherM)
import Control.Natural (type (~>))
import Network.Wai (Application)
import Servant
       ((:~>)(..), (:>), Context(..), ServantErr, Server, enter,
        serveWithContext)

import ASPico.Config (Config(..))
import ASPico.Error
       (appErrToServantErr, someExceptionToServantErr)
import ASPico.Handler.Root (ApiRoot, serverRoot)
import ASPico.Monad (ASPicoM, runASPicoM)

type Api = "v0" :> ApiRoot

-- | Given a 'Config', this returns a Wai 'Application'.
app :: Config -> Application
app config =
  let context = serverContext config
  in serveWithContext (Proxy :: Proxy Api) context $ apiServer config

-- | Given a 'Config', this returns a servant 'Server' for 'Api'
apiServer :: Config -> Server Api
apiServer config = enter naturalTrans serverRoot
  where
    naturalTrans :: ASPicoM :~> ExceptT ServantErr IO
    naturalTrans = Nat catchErrorsTransformation

    catchErrorsTransformation :: ASPicoM ~> ExceptT ServantErr IO
    catchErrorsTransformation aspicoM =
      transformation aspicoM `catches`
      [ Handler (handleErr appErrToServantErr)
      , Handler (handleErr $ someExceptionToServantErr config)
      ]

    handleErr
      :: forall e a.
         (e -> ServantErr) -> e -> ExceptT ServantErr IO a
    handleErr f exception = throwError (f exception)

    transformation :: ASPicoM ~> ExceptT ServantErr IO
    transformation aspicoM = do
      eitherRes <- liftIO $ runASPicoM config aspicoM
      fromEitherM (throwError . appErrToServantErr) eitherRes

-- | The context that will be made available to request handlers. We supply the
-- "cookie-auth"-tagged request handler defined above, so that the 'HasServer' instance
-- of 'AuthProtect' can extract the handler and run it on the request.
serverContext
  :: r -> Context '[]
serverContext _ =
  EmptyContext
