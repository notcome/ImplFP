module Language.Core.Printer where

import Data.Monoid
import Language.Core.ADT

type Line      = Name
type MultiLine = [Line]

singleton x = [x]
flatten = concatMap id

indentWithPrefix :: Line -> MultiLine -> MultiLine
indentWithPrefix prefix lines =
  let (x:xs) = lines
      first  = prefix <> x
      col    = length prefix
      indent = replicate col ' '
      rest   = map (indent <>) xs
  in first:rest

parenBlock :: MultiLine -> MultiLine
parenBlock [line] = singleton $ "(" <> line <> ")"
parenBlock block  = indentWithPrefix "(" block <> singleton ")"

printExpr :: CoreExpr -> MultiLine
printExpr fx@(EApp _ _) =
  let (f, xs) = uncurryApp fx
      f'      = printArg f
      xs'     = map printArg xs
      fx'     = f':xs'
      isMulti = 0 /= (length $ filter (>1) $ map length fx')
  in if isMulti
     then f' <> (indentWithPrefix "  " $ flatten xs')
     else singleton $ unwords $ flatten fx'
  where
    printArg x
      | isAtomicExpr x = printExpr x
      | otherwise      = parenBlock $ printExpr x
    uncurryApp fx =
      let f:xs = reverse $ unpack fx
      in (f, xs)
      where
        unpack (EApp f x) = x:(unpack f)
        unpack exp        = [exp]

printExpr (ELet isRec defs exp) =
  let letName  = if isRec then "letrec " else "let "
      defsPart = concatMap printDef defs
      expPart  = indentWithPrefix "in " $ printExpr exp
  in indentWithPrefix letName defsPart <> expPart
  where printDef (name, exp) = indentWithPrefix (name <> " = ") $ printExpr exp

printExpr (ECase exp alters) =
  let caseOf  = printCaseOf exp
      alters' = concatMap printAlter alters
  in caseOf <> indentWithPrefix "  " alters'
  where printCaseOf exp =
          let exp' = printExpr exp
          in if length exp' == 1
             then singleton $ "case " <> head exp' <> " of"
             else indentWithPrefix "case " exp' <> singleton "of"
        printAlter (tag, names, exp) =
          let match = unwords $ (show tag):names
              exp' = printExpr exp
          in indentWithPrefix (match <> " -> ") exp'

printExpr (ELam args exp) =
  let prefix = unwords $ "\\":(args <> singleton ". ")
      exp'   = printExpr exp
  in indentWithPrefix prefix exp'

printExpr x = singleton $ printAtom x

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

test = putStrLn . unlines . printExpr
