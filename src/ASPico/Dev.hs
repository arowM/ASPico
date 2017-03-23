{-# LANGUAGE TemplateHaskell #-}

{-|
Module      : ASPico.Dev
Description : functions for playing around with in GHCI

This module defines functions for playing around with in GHCI.
None of these functions should be used in real code.

All of the functions in this module are marked DEPRECATED so they show up as
warnings if they are used in real code.
-}
module ASPico.Dev
  ( module ASPico.Dev
  , module Text.Pretty.Simple
  ) where

import ASPico.Prelude hiding (many, readFile, try)

import Control.FromSum (fromEitherM)
import Data.ByteString.Base64 (encode)
import System.IO.Unsafe (unsafePerformIO)
import Text.HTML.DOM (readFile)
import Text.Pretty.Simple (pPrint)
import Text.XML (Document)
import Web.ClientSession (randomKey)

import ASPico.Config (Config, createConfig)
import ASPico.Environment (Environment(Development))
import ASPico.Error (AppErr)
import ASPico.Monad (ASPicoM, runASPicoM)

{-# WARNING
configDev, createSessionKey, runASPicoMDev, unsafeRunASPicoMDev, docFromFile
           "This function is only to be used in GHCI during development."
 #-}

configDev :: Config
configDev =
  unsafePerformIO $
  createConfig
    Development
    8082
    10
    60
    "localhost"
    5432
    "aspico"
    "ua0yay1nznznzbwer07a"
    "aspico"
    "localhost:8082"
    "http"
    "hDwxYklK+fMs1p2ycvGGh/nQb5FDJfKYI+Msk8IreM7+NWkh8nA/Kezw+xj/FnifLHvr5nXuDQV2qfQ70oKBicYYiWB+Y/lzIIARtG1ZtyEFMw1HyaUeARjG4Ue+U3kX"

createSessionKey :: IO ByteString
createSessionKey = encode . fst <$> randomKey

runASPicoMDev :: ASPicoM a -> IO (Either AppErr a)
runASPicoMDev = runASPicoM configDev

unsafeRunASPicoMDev :: ASPicoM a -> IO a
unsafeRunASPicoMDev = fromEitherM throw <=< runASPicoMDev

docFromFile :: FilePath -> IO Document
docFromFile = readFile
