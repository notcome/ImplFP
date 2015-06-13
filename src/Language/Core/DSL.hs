{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes     #-}

module Language.Core.DSL where

import Language.Haskell.TH
import Language.Haskell.TH.Quote

import Language.Core.Parser

parseCore = iparse core

coreTH :: QuasiQuoter
coreTH = QuasiQuoter
  { quoteExp = \src -> let (Right res) = parseCore src in dataToExpQ (const Nothing) res
  }
