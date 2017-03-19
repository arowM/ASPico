
{-|
Description : Http Manager

This module contains classes for working with types that have an http manager.
-}

module ASPico.Host
    ( HasHost(..)
    , HasProtocol(..)
    , WithHost(..)
    , WithProtocol(..)
    ) where

import ASPico.Prelude

-- | Class for things that have a host.  For example, "localhost",
-- "example.com", or "google.com:8080".
class HasHost a where
    getHost :: a -> Text

-- | Class for monads that have an host.  See 'HasHost'.
class Monad m => WithHost m where
    mHost :: m Text

-- | Any reader with an @r@ that 'HasHost', we can get a host.
instance (HasHost r, Monad m) => WithHost (ReaderT r m) where
    mHost :: ReaderT r m Text
    mHost = reader getHost

-- | Class for things that have a protocol.  For example, "http" or "https".
class HasProtocol a where
    getProtocol :: a -> Text

-- | Class for monads that have an protocol.  See 'HasProtocol'.
class Monad m => WithProtocol m where
    mProtocol :: m Text

-- | Any reader with an @r@ that 'HasProtocol', we can get a protocol.
instance (HasProtocol r, Monad m) => WithProtocol (ReaderT r m) where
    mProtocol :: ReaderT r m Text
    mProtocol = reader getProtocol
