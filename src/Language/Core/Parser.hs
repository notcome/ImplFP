{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}

module Language.Core.Parser where

import Control.Applicative

import Text.Parsec.Prim             (getPosition)
import Control.Monad.State

import qualified Text.Parsec        as P
import qualified Text.Parsec.Indent as PI

import Language.Core.ADT

import Debug.Trace

type IndentParser a = P.ParsecT String () (State P.SourcePos) a

indentParse' :: String -> IndentParser a -> String -> Either P.ParseError a
indentParse' src rule text = PI.runIndent src $ P.runParserT rule () src text

indentParse = indentParse' "snippet"
parseCore   = indentParse core

var = EVar <$> lowers
num = ENum <$> ((P.char '-' *> liftA negate posnum) <|> posnum)
con = P.string "Pack" *> braces pair
  where pair = liftA2 ECon posnum $ keyword "," *> posnum

app = do
  a <- withPos $ liftA2 (foldl EApp) atomicCore args
  b <- args
  trace (show $ length b) $ return a
  where args = P.endBy (indented *> atomicCore) P.spaces

letBlock = withPos $ liftA3 ELet isRec defs val
  where
    construct (rec, defs) val = ELet rec defs val
    isRec = P.string "letrec" *> return True
        <|> P.string "let"    *> return False
    def   = liftA2 (,) lowers $ keyword "=" *> core
    defs  = P.many1 $ indented *> def
    val   = keyword "in" *> core

caseBlock = withPos $ liftA2 ECase expr branches
  where
    expr   = P.between (keyword "case") (keyword "of") atomicCore
    branch = liftA3 (,,) (angles posnum)
                         (spaces *> P.endBy lowers spaces)
                         (keyword "->" *> core)
    branches = P.many1 $ indented *> branch <* P.spaces

-- Main
atom = parens core
atomicCore = choiceWithPos [atom, con, var, num]
core       = choiceWithPos [ caseBlock
                           , letBlock
                           , app
                           ]

-- Indent-Sensitive Parsec
withPos  = PI.withPos
indented = do
  ref <- P.sourceColumn <$> get
  col <- P.sourceColumn <$> getPosition
  if ref < col
  then do
    trace (show ref ++ " ok " ++ show col) $ return ()
   --return ()
  else do
    trace (show ref ++ " failed " ++ show col) $ fail "out of current block"
same     = PI.same
choiceWithPos ps = get >>= (\ref -> P.choice $ map (put ref >>) ps)
--choiceWithPos ps = get >>= choice . flip map ps . (>>) . put

-- Utilities
lowers = P.many1 P.lower
spaces = P.many $ P.char ' '
posnum = (read :: String -> Int) <$> P.many1 P.digit

braces p = P.char '{' *> p <* P.char '}'
angles p = P.char '<' *> p <* P.char '>'
parens p = P.char '(' *> p <* P.char ')'
keyword s = P.spaces *> P.string s <* P.spaces

-- Test Cases
lenCore = parseCore $ unlines [
    "case l of"
  , "  <0> x xs -> add 1 (length xs)"
  , "  <1> -> 0"]

doubleA = parseCore $ unlines [
    "let a = 3"
  , "in add a a"
  ]

bAndC = parseCore $ unlines [
    "let a = let b = 3"
  , "            c = 4"
  , "        in add b c"
  , "in add a a"]

letRecAdd = parseCore $ unlines [
    "letrec a = add b c"
  , "       b = 3"
  , "       c = 4"
  , "in add a"]
