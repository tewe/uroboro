module Uroboro.Checker
    (
      typecheck
    , findConstructor
    , findDestructor
    , findFunction
    ) where

import Data.Either (isRight)
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

    Application n es -> case (typecheck p c (FunctionApplication n es) t, typecheck p c (ConstructorApplication n es) t) of
        (Right e, Left _) -> Right e
        (Left _, Right e) -> Right e
        _ -> Left "ambiguous"

    FunctionApplication n es -> typecheckApplication p c e es t $ findFunction p n

    ConstructorApplication n es -> typecheckApplication p c e es t $ findConstructor p n

typecheckApplication :: Library -> Context -> Exp -> [Exp] -> Type -> Maybe Sig -> Either String Exp
typecheckApplication p c e es t' (Just (ts, t)) =
    if t /= t' then Left "return mismatch" else
    if length es /= length ts then Left "wrong number of arguments" else
    if all isRight $ zipWith (typecheck p c) es ts then Right e else Left "argument mismatch"
typecheckApplication _ _ _ _ _ _ = Left "unknown"
