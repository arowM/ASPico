
module Main (main) where

import ClassyPrelude

import System.FilePath.Glob (glob)
import Test.DocTest (doctest)

main :: IO ()
main = glob "src/**/*.hs" >>= doDocTest

doDocTest :: [String] -> IO ()
doDocTest options = doctest $ options <> ghcExtensions

ghcExtensions :: [String]
ghcExtensions =
    [ "-XConstraintKinds"
    , "-XDataKinds"
    , "-XDeriveDataTypeable"
    , "-XDeriveFunctor"
    , "-XDeriveGeneric"
    , "-XDuplicateRecordFields"
    , "-XEmptyDataDecls"
    , "-XFlexibleContexts"
    , "-XFlexibleInstances"
    , "-XGADTs"
    , "-XGeneralizedNewtypeDeriving"
    , "-XInstanceSigs"
    , "-XLambdaCase"
    , "-XMultiParamTypeClasses"
    , "-XNamedFieldPuns"
    , "-XNoImplicitPrelude"
    , "-XNoMonomorphismRestriction"
    , "-XOverloadedLabels"
    , "-XOverloadedLists"
    , "-XOverloadedStrings"
    , "-XPackageImports"
    , "-XPatternSynonyms"
    , "-XPolyKinds"
    , "-XRankNTypes"
    , "-XRecordWildCards"
    , "-XScopedTypeVariables"
    , "-XStandaloneDeriving"
    , "-XTupleSections"
    , "-XTypeFamilies"
    , "-XTypeOperators"
    , "-XViewPatterns"
    ]
