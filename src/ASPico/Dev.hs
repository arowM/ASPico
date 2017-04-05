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
import Servant.Client (parseBaseUrl)
import System.IO.Unsafe (unsafePerformIO)
import Text.HTML.DOM (readFile)
import Text.Pretty.Simple (pPrint)
import Text.XML (Document)

import ASPico.Config (Config, createConfig)
import ASPico.Environment (Environment(Development))
import ASPico.Error (AppErr)
import ASPico.Monad (ASPicoM, runASPicoM)

{-# WARNING
configDev, runASPicoMDev, unsafeRunASPicoMDev, docFromFile
           "This function is only to be used in GHCI during development."
 #-}

configDev :: Config
{-# NOINLINE configDev #-}
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
    "3pUiRmS2Rv6f28uW"
    "aspico"
    "localhost:8082"
    "http"
    (parseBaseUrl "http://localhost:8082/push")

runASPicoMDev :: ASPicoM a -> IO (Either AppErr a)
runASPicoMDev = runASPicoM configDev

unsafeRunASPicoMDev :: ASPicoM a -> IO a
unsafeRunASPicoMDev = fromEitherM throw <=< runASPicoMDev

docFromFile :: FilePath -> IO Document
docFromFile = readFile
