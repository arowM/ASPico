{-# OPTIONS -fno-warn-orphans #-}

module ASPico.Orphans where

import Prelude

import Control.Monad.Logger (LoggingT)
import Control.Monad.Random (MonadRandom(..))
import Control.Monad.Trans.Class (MonadTrans(lift))
import Data.Text (Text)
import Data.Semigroup (Semigroup(..))
import Text.Blaze.Internal (MarkupM)
import Text.PrettyPrint.ANSI.Leijen (Doc)
import Web.FormUrlEncoded(Form, ToForm(toForm))

instance MonadRandom m =>
         MonadRandom (LoggingT m) where
  getRandom = lift getRandom
  getRandomR = lift . getRandomR
  getRandoms = lift getRandoms
  getRandomRs = lift . getRandomRs

instance Semigroup Doc where
  (<>) :: Doc -> Doc -> Doc
  (<>) = mappend

instance ToForm () where
  toForm :: () -> Form
  toForm _ = toForm ([] :: [(Text, Text)])

instance (Monoid a) => Semigroup (MarkupM a) where
  (<>) :: MarkupM a -> MarkupM a -> MarkupM a
  (<>) = mappend
