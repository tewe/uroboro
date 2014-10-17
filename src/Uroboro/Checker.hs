module Uroboro.Checker
    (
      typecheck
    ) where

import Uroboro.Syntax

type Context = [(Identifier, Type)]

signature :: Library -> Identifier -> Maybe Signature
signature ((FunctionDefinition s@(Signature f _ _) _):xs) f' = if f == f' then Just s else signature xs f'
signature (x:xs) f = signature xs f
signature _ _ = Nothing

typecheck :: Library -> Context -> Exp -> Type -> Either String Exp
typecheck p c e t = case e of

    Variable x -> case lookup x c of
        Just t' -> if t == t' then Right e else Left "mismatch"
        Nothing -> Left "unknown"

    Application f args -> case signature p f of
        Just (Signature _ ts t') -> if t == t' then Right e else Left "mismatch"
        Nothing -> Left "unknown"
