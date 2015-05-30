{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}

import Data.Maybe                   (isJust)
import Control.Monad.State          (State)

import qualified Text.Parsec        as P
import qualified Text.Parsec.Indent as PI

import Language

import System.IO.Unsafe

type IParser a = P.ParsecT String () (State P.SourcePos) a
iparse :: IParser a -> String -> Either P.ParseError a
iparse rule text = PI.runIndent "(source)" $ P.runParserT rule () "(source)" text

parse rule text = P.parse rule "(source)" text

spaces = P.spaces >> PI.indented >> P.spaces
--spaces = P.spaces >> PI.indented
--spaces = PI.indented >> P.spaces

var = do name <- P.many1 P.lower
         return $ EVar name

num = do neg <- fmap isJust $ P.optionMaybe $ P.char '-'
         spaces
         num <- parseNum
         return $ ENum $ if neg then -num else num

-- identifier starting with upper letter must be a constructor in Haskell
con = do P.string "Pack{" >> spaces
         id    <- parseNum
         spaces >> P.char ',' >> spaces
         arity <- parseNum
         spaces >> P.char '}'
         return $ ECon id arity

app = do fn   <- atomicCore
         spaces
         --args <- P.endBy (PI.indented >> atomicCore) (PI.indented >> P.spaces)
         args <- P.sepBy atomicCore spaces'
         if length args == 0
         then return fn
         else return $ foldl EApp fn args

caseBlock = do block <- PI.withBlock ECase caseExpr caseBranch
               spaces
               return block
  where
    caseExpr = do P.string "case" >> spaces
                  expr <- atomicCore
                  spaces >> P.string "of" >> spaces
                  return expr
    caseBranch = do
      id    <- parseNum
      P.spaces
      binds <- P.endBy var spaces
      spaces >> P.string "->" >> spaces
      expr  <- core
      P.spaces
      return $ (id, binds, expr)

atom = do P.char '(' >> spaces
          expr <- core
          spaces >> P.char ')'
          return expr

atomicCore = P.choice [atom, con, var, num]
core       = P.choice [caseBlock, app]

parseNum = readNum <$> P.many1 P.digit
  where readNum x = (read x) :: Int

lenCore = iparse caseBlock $ unlines [
    "case l of"
  , "  0 x xs -> add 1"-- (length xs)"
  , "  1 -> 0"]
