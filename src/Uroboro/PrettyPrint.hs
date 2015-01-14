{-|
Description : User-readable syntax trees

We want interpreter output (reduced expressions) to look like input.
-}
module Uroboro.PrettyPrint where

import Data.List (intercalate)

import Uroboro.Tree (Type(Type), TExp(..))

parens :: String -> String
parens s = "(" ++ s ++ ")"

commaSep :: [String] -> String
commaSep ss = intercalate ", " ss

args :: [TExp] -> String
args es = parens (commaSep (map render es))

render :: TExp -> String
render (TVar _ n)          = n
render (TApp _ f as)       = f ++ args as
{-
render e@(TCon (Type "Int") _ _) = show $ sum e
  where
    sum (TCon _ "zero" _) = 0
    sum (TCon _ "succ" [a]) = 1 + sum a
-}
render (TCon _ c as)       = c ++ args as
render (TDes _ d as inner) = render inner ++ "." ++ d ++ args as
