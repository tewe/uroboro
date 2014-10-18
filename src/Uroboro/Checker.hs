module Uroboro.Checker
    (
      typecheck
    , findConstructor
    , findDestructor
    , findFunction
    ) where

import Data.List (find)
import Data.Maybe (listToMaybe)

import Uroboro.Syntax

type Context = [(Identifier, Type)]
type Sig = ([Type], Type)

signature :: Library -> Identifier -> Maybe Signature
signature ((FunctionDefinition s@(Signature f _ _) _):xs) f' = if f == f' then Just s else signature xs f'
signature (x:xs) f = signature xs f
signature _ _ = Nothing

constructors :: Definition -> [Signature]
constructors (DataDefinition _ ss) = ss
constructors _ = []

findConstructor :: Library -> Identifier -> Maybe Sig
findConstructor p c = findS (concatMap constructors p) c

destructors :: Definition -> [Signature]
destructors (CodataDefinition _ ss) = ss
destructors _ = []

findDestructor :: Library -> Type -> Identifier -> Maybe Sig
findDestructor p t s = findS (concat [ss | (CodataDefinition n ss) <- p, n == t]) s

findFunction :: Library -> Identifier -> Maybe Sig
findFunction p f = listToMaybe [(ts, t) | (FunctionDefinition (Signature f' ts t) _) <- p, f' == f]

findS :: [Signature] -> Identifier -> Maybe Sig
findS ss n = listToMaybe [(ts, t) | Signature n' ts t <- ss, n' == n]

typecheck :: Library -> Context -> Exp -> Type -> Either String Exp
typecheck p c e t = case e of

    Variable x -> case lookup x c of
        Just t' -> if t == t' then Right e else Left "mismatch"
        Nothing -> Left "unknown"

    Application f args -> case signature p f of
        Just (Signature _ ts t') -> if t == t' then Right e else Left "mismatch"
        Nothing -> Left "unknown"
