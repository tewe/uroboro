module Uroboro.Checker where

import Data.List (find)

import Uroboro.Tree

-- |For foldlM
pos :: [PT] -> PT -> Either String [PT]
pos defs def@(PTPos t constructors)
    | find (mismatch t) constructors /= Nothing = Left $
        "Definition Mismatch: " ++ t ++ " is not returned by all of its constructors"
    | find (existing t) defs /= Nothing         = Left $
        "Shadowed Definition: " ++ t ++ " is defined more than once"
    | otherwise                                 = Right (def:defs)
  where
    existing name (PTPos n _) = name == n
    existing name (PTNeg n _) = name == n
    existing _ _ = False

    mismatch name (PTCon _ _ n) = name /= n
pos _ _ = return []
