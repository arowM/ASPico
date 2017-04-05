module ASPico.Handler.Root.RegisterPush
  ( ApiRegisterPush
  , serverRegisterPush
  ) where

import ASPico.Prelude hiding (product)

import Database.Persist.Sql (Entity(..), fromSqlKey)
import Servant ((:>), FormUrlEncoded, JSON, Post, ReqBody, ServerT)

import ASPico.Envelope (Envelope, returnSuccess)
import ASPico.Error (AppErr)
import ASPico.Form (RegisterPushForm(..), RegisterPushResp(..))
import ASPico.Monad (MonadASPicoDb, dbCreatePush)

type ApiRegisterPush = "register" :> "push" :> ReqBody '[JSON, FormUrlEncoded] RegisterPushForm :> Post '[JSON, FormUrlEncoded] (Envelope RegisterPushResp)

serverRegisterPush
  :: (MonadError AppErr m, MonadASPicoDb m)
  => ServerT ApiRegisterPush m
serverRegisterPush = registerPush

registerPush
  :: (MonadError AppErr m, MonadASPicoDb m)
  => RegisterPushForm -> m (Envelope RegisterPushResp)
registerPush form = do
  Entity k _ <- dbCreatePush form
  returnSuccess . RegisterPushResp . tshow . fromSqlKey $ k
