{-# LANGUAGE UndecidableInstances #-}

module ASPico.Monad
    ( module X
    , module ASPico.Monad
    ) where

import ASPico.Monad.Base as X
import ASPico.Monad.Db as X
import ASPico.Monad.Http as X

-- | This constraint synonym wraps up all of our ASPico type classes.
type MonadASPico m =
    ( MonadASPicoDb m
    , MonadASPicoHttp m
    )
