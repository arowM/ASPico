{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module ASPico.Handler.Root.Conversion.TH
  ( pngContent
  ) where

import ASPico.Prelude

import Language.Haskell.TH (Exp, Q)
import Language.Haskell.TH.Syntax (addDependentFile, runIO)

dummyPng :: FilePath
dummyPng = "templates" </> "dummy.png"

pngContent :: Q Exp
pngContent = do
  addDependentFile dummyPng
  (fileContent :: ByteString) <- runIO . readFile $ dummyPng
  [|fileContent|]
