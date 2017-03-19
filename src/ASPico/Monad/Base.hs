module ASPico.Monad.Base where

import ASPico.Prelude

import Control.Monad.Logger ( runStdoutLoggingT )
import Control.Monad.Random ( MonadRandom )
import Control.Monad.Time ( MonadTime )
import Control.Monad.Trans.Control ( MonadBaseControl(..) )

import ASPico.Config ( Config )
import ASPico.Error ( AppErr )

-- | Monad transformer stack for our application.
newtype ASPicoM a = ASPicoM
    { unASPicoM ::
        ReaderT Config (ExceptT AppErr (LoggingT IO)) a
    }
    deriving
        ( Applicative
        , Functor
        , Monad
        , MonadBase IO
        , MonadError AppErr
        , MonadIO
        , MonadLogger
        , MonadRandom
        , MonadReader Config
        , MonadTime
        )

instance MonadBaseControl IO ASPicoM where
    type StM ASPicoM a = Config -> IO (Either AppErr a)

    liftBaseWith
        :: forall a
         . ((forall x .  ASPicoM x -> IO (Config -> IO (Either AppErr x))) -> IO a)
        -> ASPicoM a
    liftBaseWith f = liftBase $ f unwrapASPicoM
      where
        unwrapASPicoM
            :: forall z . ASPicoM z -> IO (Config -> IO (Either AppErr z))
        unwrapASPicoM aspicoM =
            pure $ \config -> runASPicoM config aspicoM

    restoreM :: forall a . (Config -> IO (Either AppErr a)) -> ASPicoM a
    restoreM f = ASPicoM readerT
      where
        readerT :: ReaderT Config (ExceptT AppErr (LoggingT IO)) a
        readerT = ReaderT $ \config -> ExceptT . lift $ f config

-- | Run the 'ASPicoM' monad stack.
runASPicoM :: Config -> ASPicoM a -> IO (Either AppErr a)
runASPicoM config =
      runStdoutLoggingT
    . runExceptT
    . flip runReaderT config
    . unASPicoM
