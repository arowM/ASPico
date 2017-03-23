{-|
Description : Envelope type used to return responses from API

This module contains the 'Envelope' type, which wraps responses from the API.
-}

module ASPico.Envelope
    ( module ASPico.Envelope
    , Envelope.Err(..)
    , Envelope.throwEnvelopeErr
    , Envelope.toSuccessEnvelope
    ) where

import ASPico.Prelude

import qualified Web.Envelope as Envelope

import ASPico.Error (AppErrEnum)

type Envelope a = Envelope.Envelope AppErrEnum a

returnSuccess :: (Applicative m) => a -> m (Envelope a)
returnSuccess = pure . Envelope.toSuccessEnvelope
