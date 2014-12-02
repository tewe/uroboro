module Uroboro.Checker where

import Data.List (find, intercalate)

import Uroboro.Tree

-- |Fold over positive type definitions
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

-- |Fold to get defined types
types :: [Type] -> PT -> [Type]
types ts (PTPos t _) = (t:ts)
types ts (PTNeg t _) = (t:ts)
types ts (PTFun _ _ _ _) = ts

-- |Fold to get argument types
desArgTypes :: [Type] -> PTDes -> [Type]
desArgTypes ts (PTDes _ _ args _) = ts ++ args

-- |Fold over negative type definitions
neg :: [PT] -> PT -> Either String [PT]
neg defs def@(PTNeg t destructors)
    | t `elem` defined = Left $
        "Shadowed Definition: " ++ t ++ " is defined more than once"
    | any (flip notElem (t:defined)) args = Left $
        "Missing Definition: " ++ argString ++ " are not all defined"
    | otherwise = Right (def:defs)
  where
    defined = foldl types [] defs
    args = foldl desArgTypes [] destructors
    argString = intercalate ", " args
neg _ _ = return []
