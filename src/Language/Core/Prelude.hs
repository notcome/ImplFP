{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes     #-}

module Language.Core.Prelude where

import Language.Core.ADT
import Language.Core.DSL

preludeDefs :: CoreProgram
preludeDefs = [
    ("I", ["x"], EVar "x")
  , ("K", ["x", "y"], EVar "x")
  , ("K1", ["x", "y"], EVar "y")
  , ("S", ["f", "g", "x"], [coreTH|f x (g x)|])
  , ("compose", ["f", "g", "x"], [coreTH|f (g x)|])
  , ("twice", ["f", "g", "x"], [coreTH|compose f f|])
  ]
