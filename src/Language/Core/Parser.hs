{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}

module Language.Core.Parser (parseCore) where

import Control.Applicative
import Control.Monad.State
import qualified Text.Parsec        as P
import qualified Text.Parsec.Indent as PI

import Language.Core.ADT

type IndentParser a = P.ParsecT String () (State P.SourcePos) a

indentParse' :: String -> IndentParser a -> String -> Either P.ParseError a
indentParse' src rule text = PI.runIndent src $ P.runParserT rule () src text

indentParse = indentParse' "snippet"
parseCore   = indentParse core

var = P.try (lowers >>= check)
  where check a | a `elem` keywords = fail "a keyword"
                | otherwise         = return $ EVar a 
num = ENum <$> ((P.char '-' *> liftA negate posnum) <|> posnum)
con = P.string "Pack" *> braces pair
  where pair = liftA2 ECon posnum $ keyword "," *> posnum

app = withPos $ liftA2 (foldl EApp) applicable args
  where args = P.spaces *> P.endBy (indented *> atom) P.spaces

let_ = withPos $ liftA3 ELet isRec defs val
  where
    construct (rec, defs) val = ELet rec defs val
    isRec = keyword "letrec" *> return True
        <|> keyword "let"    *> return False
    def   = liftA2 (,) lowers $ keyword "=" *> core
    defs  = P.many1 $ indented *> def
    val   = keyword "in" *> core

case_ = withPos $ liftA2 ECase expr branches
  where
    expr   = P.between (keyword "case") (keyword "of") atom
    branch = liftA3 (,,) (angles posnum)
                         (spaces *> P.endBy lowers spaces)
                         (keyword "->" *> core)
    branches = P.many1 $ indented *> branch <* P.spaces

lam = withPos $ liftA2 ELam (keyword "\\" *> P.endBy lowers spaces)
                            (keyword "->" *> core)
-- Main
atomized = parens core
atom     = choiceWithPos [num, atomized, con, var]
core     = choiceWithPos [num, case_, let_, lam, app]

applicable = P.try (atom >>= check)
  where check (ENum _) = fail "numbers not applicable"
        check a        = return a

-- Indent-Sensitive Parsec
withPos  = PI.withPos
indented = do
  ref <- P.sourceColumn <$> get
  col <- P.sourceColumn <$> P.getPosition
  if ref < col
  then return ()
  else fail "out of current block"
same     = PI.same
choiceWithPos ps = get >>= P.choice . flip map ps . (>>) . put

-- Utilities
lowers = P.many1 P.lower
spaces = P.many $ P.char ' '
posnum = (read :: String -> Int) <$> P.many1 P.digit

braces p = P.char '{' *> p <* P.char '}'
angles p = P.char '<' *> p <* P.char '>'
parens p = P.char '(' *> p <* P.char ')'
keyword s = P.spaces *> P.try (P.string s) <* P.spaces

keywords = ["let", "letrec", "in", "case", "of"] 

