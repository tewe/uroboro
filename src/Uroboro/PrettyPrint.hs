module Uroboro.PrettyPrint where

import Data.List (intercalate)

import Uroboro.Tree (TExp(..))

parens :: String -> String
parens s = "(" ++ s ++ ")"

commaSep :: [String] -> String
commaSep ss = intercalate ", " ss

args :: [TExp] -> String
args es = parens (commaSep (map render es))

render :: TExp -> String
render (TVar _ n)          = n
render (TApp _ f as)       = f ++ args as
render (TCon _ c as)       = c ++ args as
render (TDes _ d as inner) = render inner ++ "." ++ d ++ args as
