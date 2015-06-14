module Language.Core.Printer where

import Text.PrettyPrint.Leijen
import Prelude hiding ((<$>))

import Language.Core.ADT

atom :: CoreExpr -> Doc
atom (EVar v) = text v
atom (ENum n) = text $ show n
atom (ECon tag arity) = text "Pack" <> braces (int tag <> text ", " <> int arity)
atom x = parens $ expr x

expr :: CoreExpr -> Doc
expr (ELet isRec binds exp) = let
    let_   = text $ if isRec then "letrec" else "let"
    binds' = align $ vsep $ map bind binds
    in_    = text "in"
    exp'   = expr exp
  in align (let_ <+> binds' <$> in_ <+> exp')
  where bind (name, exp) = text name <+> text "=" <+> expr exp

expr (ECase exp alters) = let
    exp'    = atom exp
    match   = text "case" <+> exp' <+> text "of"
    alters' = vsep $ map (indent 2 . alter) alters
  in align (match <$> alters')
  where alter (tag, names, exp) = let
            tag'   = angles $ int tag
            names' = hsep $ map text names
            exp'   = expr exp
          in if null names
             then tag'            <+> text "->" <+> exp'
             else tag' <+> names' <+> text "->" <+> exp'

expr (ELam names exp) = let
    names' = hsep $ map text names
    quant  = text "\\" <> names' <> text "."
    exp'   = expr exp
  in align (quant <+> exp')

expr fx@(EApp _ _) = let
    (f, xs)   = uncurryApp fx
    f'        = atom f
    xs'       = align $ sep $ map atom xs
  in align $ f' <+> xs'
  where
    uncurryApp fx = uncons $ reverse $ unpack fx
      where
        uncons (x:xs)     = (x, xs)
        unpack (EApp f x) = x:(unpack f)
        unpack x          = [x]

expr x = atom x

putExpr e = (putDoc $ expr e) >> putStrLn ""

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
