{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}

module Language.Core.Parser where

import Data.Maybe                   (isJust)
import Text.Parsec                  ((<|>))
import Text.Parsec.Prim             (getPosition)

import Control.Monad.State

import qualified Text.Parsec        as P
import qualified Text.Parsec.Indent as PI

import Language.Core.ADT

type IParser a = P.ParsecT String () (State P.SourcePos) a

iparse :: IParser a -> String -> Either P.ParseError a
iparse rule text = PI.runIndent "(source)" $ P.runParserT rule () "(source)" text

parse rule text = P.parse rule "(source)" text

{-
 - Atoms = EVar Name
 -       | ENum Int
 -       | ECon Int Int
 -}

var = EVar <$> lowers

num = do neg <- P.optionMaybe $ P.char '-'
         num <- parseNum
         return $ ENum $ case neg of
                           Nothing -> num
                           Just _  -> -num

con = do P.string "Pack{" >> spaces
         id    <- parseNum
         spaces >> P.char ',' >> spaces
         arity <- parseNum
         spaces >> P.char '}'
         return $ ECon id arity

{-
 - Complex = EApp (Expr a) (Expr a)
 -         | ELet
 -             IsRec
 -             [(a, Expr a)]
 -             (Expr a)
 -         | ECase
 -             (Expr a)
 -             [Alter a]
 -}

biAp f o a b = f a `o` f b

checkBlock = do
  s <- get
  p <- getPosition
  if biAp P.sourceColumn (<) s p
    then return ()
    else fail "out of current block"

app = PI.withPos $ do
  fn <- atomicCore
  P.spaces
  args <- P.endBy (checkBlock >> atomicCore) (P.spaces)
  case args of
    [] -> return fn
    _  -> return $ foldl EApp fn args

letBlock = PI.withPos $ do
    (isRec, defs) <- letDefs
    value         <- PI.checkIndent >> letValue
    return $ ELet isRec defs value
  where
    letSym = do
      symbol <- P.string "letrec" <|> P.string "let"
      P.spaces
      if symbol == "let"
      then return False
      else return True
    letDef = do
      bind  <- lowers
      P.spaces >> P.char '=' >> P.spaces
      value <- core
      return (bind, value)
    letDefs = PI.withBlock (,) letSym letDef
    letValue = P.string "in" >> P.spaces >> core

caseBlock = PI.withBlock ECase caseExpr caseBranch
  where
    caseExpr = do P.try $ P.string "case" >> P.spaces
                  expr <- atomicCore
                  P.spaces >> P.string "of" >> P.spaces
                  return expr

    caseBranch = do
      id    <- parseNum
      spaces
      binds <- P.endBy lowers spaces
      spaces >> P.string "->" >> spaces
      expr  <- core
      P.spaces
      return (id, binds, expr)

-- Main
atom = do P.char '(' >> spaces
          expr <- core
          spaces >> P.char ')'
          return expr

atomicCore = choiceWithPos [atom, con, var, num]
core       = choiceWithPos [ caseBlock
                           , letBlock
                           , app
                           ]

-- Fix "bugs"
choiceWithPos ps = do
  a <- get
  let ps' = map (put a >>) ps
  P.choice ps'

-- Utilities
lowers = P.many1 P.lower
spaces = P.many $ P.char ' '
parseNum = readNum <$> P.many1 P.digit
  where readNum x = (read x) :: Int

-- Test Cases
lenCore = iparse caseBlock $ unlines [
    "case l of"
  , "  0 x xs -> add 1 (length xs)"
  , "  1 -> 0"]

doubleA = iparse letBlock $ unlines [
    "let a = 3"
  , "in add a a"
  ]

bAndC = iparse letBlock $ unlines [
    "let a = let b = 3"
  , "            c = 4"
  , "        in add b c"
  , "in add a a"]

letRecAdd = iparse letBlock $ unlines [
    "letrec a = add b c"
  , "       b = 3"
  , "       c = 4"
  , "in add a"]
