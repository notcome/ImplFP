{-# LANGUAGE FlexibleContexts #-}

module Language.Core.Printer where

import Data.Monoid
import Control.Monad.Writer
import Language.Core.ADT

type Line      = Name

data MultiLine = ML [Line] deriving Show
singleton = ML

same' :: [MultiLine] -> MultiLine
same' xs' = let
    xs = map (\(ML x) -> x) xs'
  in ML $ foldr (<>) [] xs

same :: [MultiLine] -> MultiLine
same xs' = let
    xs = map (\(ML x) -> x) xs'
    collapsable = (== []) $ filter (\x -> length x > 1) xs
  in if collapsable
     then ML [unwords $ map head xs]
     else ML $ foldr (<>) [] xs

instance Monoid MultiLine where
  mempty      = ML []
  mappend (ML []) bs = bs
  mappend as (ML []) = as
  mappend (ML as) (ML bs) = let
      (a:as') = reverse as
      as''    = reverse as'
      abs     = indentWithPrefix (a <> " ") bs
    in ML (as'' <> abs)

indentWithPrefix :: Line -> [Line] -> [Line]
indentWithPrefix prefix lines =
  let (x:xs) = lines
      first  = prefix <> x
      col    = length prefix
      indent = replicate col ' '
      rest   = map (indent <>) xs
  in first:rest

atomize xs = ML ["("] <> xs <> ML [")"]

writ str = tell $ ML [str]

printExpr :: CoreExpr -> MultiLine
printExpr (EApp f x)             = printApp  $ EApp f x
printExpr (ELet isRec binds exp) = printLet  isRec binds exp
printExpr (ECase exp alters)     = printCase exp alters
printExpr (ELam names exp)       = printLam  names exp
printExpr x                      = ML [printAtom x]

printExpr' x
  | isAtomicExpr x = printExpr x
  | otherwise      = atomize $ printExpr x

uncurryApp fx = let f:xs = reverse $ unpack fx in (f, xs)
  where
    unpack (EApp f x) = x:(unpack f)
    unpack exp        = [exp]

printApp fx = let
    (f, xs) = uncurryApp fx
  in execWriter $ do
    tell $ printExpr f
    tell $ same $ map printExpr' xs

printLet isRec binds exp = let
    binds' = execWriter $ printBinds isRec binds
    exp'   = execWriter $ printIn exp
  in same' [binds', exp']
  where
    printBind (name, exp) = do
      writ name
      writ "="
      tell $ printExpr exp
    printBinds isRec binds = do
      writ $ if isRec then "letrec" else "let"
      tell $ same' $ map (execWriter . printBind) binds
    printIn exp = do writ "in"
                     tell $ printExpr exp

printCase exp alters = let
    caseOf  = execWriter $ printCaseOf exp
    alters' = execWriter $ do
      writ " "
      tell $ same' $ map (execWriter . printAlter) alters
  in same' [caseOf, alters']
  where
    printCaseOf exp = do
      writ "case"
      tell $ printExpr exp
      writ "of"
    printAlter (tag, names, exp) = do
      writ $ show tag
      mapM writ names
      writ "->"
      tell $ printExpr exp

printLam names exp = execWriter $ do
  writ "\\"
  mapM writ names
  writ "."
  tell $ printExpr exp

printAtom :: CoreExpr -> String
printAtom (EVar v) = v
printAtom (ENum n) = show n
printAtom (ECon tag arity) = "Pack{" <> show tag <> ", " <> show arity <> "}"

{-
let a = let b = 3
            c = 4
        in add b c
in add a a
-}
bAndC :: CoreExpr
bAndC = ELet False
             [("b", ENum 3)
             ,("c", ENum 4)]
             (EApp (EApp (EVar "add") (EVar "b")) (EVar "c"))

doubleA = ELet True
               [("a", bAndC)]
               (EApp (EApp (EVar "add") (EVar "a")) (EVar "a"))

{-
case l of
  0 x xs -> add 1 (length xs)
  1 -> 0
-}
lenCore = ECase (EVar "l")
                [(0, ["x", "xs"], EApp (EApp (EVar "add") (ENum 1)) (EApp (EVar "length") (EVar "xs")))
                ,(1, [], ENum 0)]

{-
\ f . (\ x . f (x x)) (\ x . f (x x))
-}
yComb = ELam ["f"] (EApp part part)
  where f    = EVar "f"
        x    = EVar "x"
        part = ELam ["x"] (EApp f (EApp x x))

{-
:  === Pack{0, 2}
[] === Pack{1, 0}
length (: 1 (: 2 (: 3 [])))
-}
appLen = EApp (EVar "length") list
  where
    con x xs  = EApp (EApp (ECon 0 2) x) xs
    nil       = ECon 1 0
    [a, b, c] = map ENum [1..3]
    list      = con a (con b (con c nil))
