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

isF :: Definition -> Bool
isF (FunctionDefinition _ _) = True
isF _ = False

isD :: Definition -> Bool
isD (DataDefinition _ _) = True
isD _ = False

isC :: Definition -> Bool
isC (CodataDefinition _ _) = True
isC _ = False

constructors :: Definition -> [Signature]
constructors (DataDefinition _ ss) = ss
constructors _ = []

findConstructor :: Library -> Identifier -> Maybe Sig
findConstructor p c = findS (concatMap constructors p) c

findD :: Library -> Identifier -> Maybe Definition
findD p n = find (\d -> dName d == n) p

destructors :: Definition -> [Signature]
destructors (CodataDefinition _ ss) = ss
destructors _ = []

dNamed :: [Definition] -> Identifier -> [Definition]
dNamed p n = filter (\d -> dName d == n) p

findDestructor :: Library -> Type -> Identifier -> Maybe Sig
findDestructor p t s = findS (concatMap destructors (dNamed p t)) s

findF :: Library -> Identifier -> Maybe Definition
findF p f = findD (filter isF p) f

fType :: Definition -> Maybe Sig
fType (FunctionDefinition (Signature _ ts t) _) = Just (ts, t)
fType _ = Nothing

findFunction :: Library -> Identifier -> Maybe Sig
findFunction d f = findF d f >>= fType

sName :: Signature -> Identifier
sName (Signature n _ _) = n

dName :: Definition -> Identifier
dName (DataDefinition n _) = n
dName (CodataDefinition n _) = n
dName (FunctionDefinition s _) = sName s

sType :: Signature -> Sig
sType (Signature _ ts t) = (ts, t)

findS :: [Signature] -> Identifier -> Maybe Sig
{-
findS ss n = case find (\s -> sName s == n) ss of
    Just (Signature _ ts t) -> Just (ts, t)
    _ -> Nothing
findS ss n = do
    s <- find (\s -> sName s == n) ss
    return $ sType s
findS ss n = find (\s -> sName s == n) ss >>= return . sType
-}
findS ss n = listToMaybe [(ts, t) | Signature n' ts t <- ss, n' == n]

typecheck :: Library -> Context -> Exp -> Type -> Either String Exp
typecheck p c e t = case e of

    Variable x -> case lookup x c of
        Just t' -> if t == t' then Right e else Left "mismatch"
        Nothing -> Left "unknown"

    Application f args -> case signature p f of
        Just (Signature _ ts t') -> if t == t' then Right e else Left "mismatch"
        Nothing -> Left "unknown"
