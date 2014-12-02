module Uroboro.Checker where

import Data.List (find, intercalate)

import Uroboro.Tree

type Context = [(Identifier, Type)]

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
    | t `elem` defined                    = Left $
        "Shadowed Definition: " ++ t ++ " is defined more than once"
    | any (flip notElem (t:defined)) args = Left $
        "Missing Definition: " ++ argString ++ " are not all defined"
    | otherwise                           = Right (def:defs)
  where
    defined = foldl types [] defs
    args = foldl desArgTypes [] destructors
    argString = intercalate ", " args
neg _ _ = return []

-- |Infer a term's type
expInfer :: [PT] -> Context -> PExp -> Either String TExp
expInfer _ vars (PVar name) = case lookup name vars of
    Just t  -> return (TVar t name)
    Nothing -> Left $ "Unknown Variable: " ++ name ++ " isn't bound"
expInfer defs vars (PApp name args) = Left "TODO"
expInfer defs vars (PDes name args inner) = do
    fromType <- expInfer defs vars inner
    Left "TODO"

-- |Typecheck a term
exp :: [PT] -> Context -> PExp -> Type -> Either String TExp
exp defs vars (PVar name) returnType = Left "TODO"
exp defs vars (PApp name args) returnType = Left "TODO"
exp defs vars (PDes name args inner) returnType = Left "TODO"
