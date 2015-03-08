{-|
Description : User-readable syntax trees

We want interpreter output (reduced expressions) to look like input.
-}
module Uroboro.PrettyPrint where

import Data.List (intercalate)

import Uroboro.Tree.Internal (Type(Type), Exp(..))

parens :: String -> String
parens s = "(" ++ s ++ ")"

commaSep :: [String] -> String
commaSep ss = intercalate ", " ss

args :: [Exp] -> String
args es = parens (commaSep (map render es))

render :: Exp -> String
render (VarExp _ n)          = n
render (AppExp _ f as)       = f ++ args as
{-
render e@(Con (Type "Int") _ _) = show $ sum e
  where
    sum (Con _ "zero" _) = 0
    sum (Con _ "succ" [a]) = 1 + sum a
-}
render (ConExp _ c as)       = c ++ args as
render (DesExp _ d as inner) = render inner ++ "." ++ d ++ args as
