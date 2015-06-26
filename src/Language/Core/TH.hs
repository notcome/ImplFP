{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes     #-}

module Language.Core.TH where

import Language.Haskell.TH
import Language.Haskell.TH.Quote

import Language.Core.Parser

core :: QuasiQuoter
core = QuasiQuoter { quoteExp = toExpQ . parseCore }
  where toExpQ (Right ast) = dataToExpQ (const Nothing) ast
