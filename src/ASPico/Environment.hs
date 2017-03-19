{-|
Description : Environment

This module contains a definition for the different environments we will
support.
-}

module ASPico.Environment where

import ASPico.Prelude

data Environment = Development | Test | Production
  deriving (Eq, Show, Read)

-- | Class for things that have an 'Environment'.
class HasEnv a where
    getEnv :: a -> Environment

instance HasEnv Environment where
    getEnv :: Environment -> Environment
    getEnv = id

-- | Class for monads that have an 'Environment'.
class WithEnv m where
    mEnv :: m Environment

instance (HasEnv r, Monad m) => WithEnv (ReaderT r m) where
    mEnv :: ReaderT r m Environment
    mEnv = reader getEnv

