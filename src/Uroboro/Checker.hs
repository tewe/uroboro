module Uroboro.Checker
    (
      typecheck
    ) where

import Uroboro.Syntax

type Context = [(Identifier, Type)]

typecheck :: Library -> Context -> Exp -> Type -> Either String Exp
typecheck p c e t = case e of
    Variable x -> case lookup x c of
        Just t' -> if t == t' then Right e else Left "mismatch"
        Nothing -> Left "unknown"
