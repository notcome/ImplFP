module Language.Core.ADT where

import Prelude hiding (print)
import Data.Monoid

data Expr a = EVar Name
            | ENum Int
            | ECon Int Int
            | EApp (Expr a) (Expr a)
            | ELet
                IsRec
                [(a, Expr a)]
                (Expr a)
            | ECase
                (Expr a)
                [Alter a]
            | ELam [a] (Expr a)
            deriving (Eq, Show)

type CoreExpr = Expr Name
type Name     = String

type IsRec   = Bool
recursive    = True
nonRecursive = False

bindersOf :: [(a, b)] -> [a]
bindersOf defns = [name | (name, rhs) <- defns]
rhssOf    :: [(a, b)] -> [b]
rhssOf    defns = [rhs  | (name, rhs) <- defns]

type Alter a = (Int, [a], Expr a)
type CoreAlt = Alter Name

isAtomicExpr :: Expr a -> Bool
isAtomicExpr (EVar _)   = True
isAtomicExpr (ENum _)   = True
isAtomicExpr (ECon _ _) = True
isAtomicExpr _          = False

type Program a   = [ScDefn a]
type CoreProgram = Program Name

type ScDefn a   = (Name, [a], Expr a)
type CoreScDefn = ScDefn Name

{-
 - I x = x
 - K x y = x
 - K1 x y = y
 - S f g x = f x (g x)
 - compose f g x = f (g x)
 - twice f = compose f f
 -}

preludeDefs :: CoreProgram
preludeDefs = [
    ("I", ["x"], EVar "x")
  , ("K", ["x", "y"], EVar "x")
  , ("K1", ["x", "y"], EVar "y")
  , ("S", ["f", "g", "x"], EApp (EApp (EVar "f") (EVar "x"))
                                (EApp (EVar "g") (EVar "x")))
  , ("compose", ["f", "g", "x"], EApp (EVar "f")
                                      (EApp (EVar "g") (EVar "x")))
  , ("twice", ["f", "g", "x"], EApp (EApp (EVar "compose") (EVar "f")) (EVar "f"))
  ]
