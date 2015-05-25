{-|
Description : Evaluate Uroboro

Define the operational semantics and reduce terms.
-}
module Uroboro.Interpreter
    (
      eval
    , pmatch
    ) where

import Control.Monad (zipWithM)
import Data.Either (rights)

import Uroboro.Tree.Internal
    (
      Identifier
    , Rule
    , Rules
    , Exp(..)
    , Pat(..)
    , Cop(..)
    , Type(..)
    )

-- |Evaluation contexts.
data E = EApp Type [Exp]
       | EDes Type Identifier [Exp] E deriving (Show, Eq)

-- |Result of a pattern match.
type Substitution = [(Identifier, Exp)]

-- |Pattern matching.
pmatch :: Exp -> Pat -> Either String Substitution
pmatch (VarExp _ _) _          = error "Substitute variables before trying to match"
pmatch term (VarPat r x)
    | returnType term /= r   = Left "Type Mismatch"
    | otherwise              = return [(x, term)]
  where
    returnType (VarExp t _)     = t
    returnType (AppExp t _ _)   = t
    returnType (ConExp t _ _)   = t
    returnType (DesExp t _ _ _) = t
pmatch (ConExp r c ts) (ConPat r' c' ps)
    | r /= r'                = Left "Type Mismatch"
    | c /= c'                = Left $
        "Name Mismatch: constructor " ++ c' ++ " doesn't match pattern " ++ c
    | length ts /= length ps = Left "Argument Length Mismatch"
    | otherwise              = zipWithM pmatch ts ps >>= return . concat
pmatch _ _                   = Left "Not Comparable"

-- |Copattern matching.
qmatch :: E -> Cop -> Either String Substitution
qmatch (EApp r as) (AppCop r' _ ps)
    | r /= r'                = error "Type checker guarantees hole type"
    | length as /= length ps = Left "Argument Length Mismatch"
    | otherwise              = zipWithM pmatch as ps >>= return . concat
qmatch (EDes r d as inner) (DesCop r' d' ps inner')
    | r /= r'                = Left "Type Mismatch"
    | d /= d'                = Left "Name Mismatch"
    | length as /= length ps = Left "Argument Length Mismatch"
    | otherwise              = do
        is <- qmatch inner inner'
        ss <- zipWithM pmatch as ps
        return $ is ++ (concat ss)
qmatch _ _                   = Left "Not Comparable"

-- |Substitute all occurences.
subst :: Exp -> (Identifier, Exp) -> Exp
subst t@(VarExp _ n') (n, term)
    | n == n'               = term
    | otherwise             = t
subst (AppExp t n as) s       = AppExp t n $ map (flip subst s) as
subst (ConExp t n as) s       = ConExp t n $ map (flip subst s) as
subst (DesExp t n as inner) s = DesExp t n (map (flip subst s) as) (subst inner s)

-- |If context matches rule, apply it.
contract :: E -> Rule -> Either String Exp
contract context (pattern, term) = do
    s <- qmatch context pattern
    return $ foldl subst term s

-- |Find hole.
reducible :: Exp -> Either String (E, Identifier)  -- Could be Maybe.
reducible (AppExp r f args) = return (EApp r args, f)
reducible (DesExp r d args inner) = do
    (inner', f) <- reducible inner
    return (EDes r d args inner', f)
reducible t = Left $ "Not a redex: " ++ show t

-- |Star reduction.
eval :: Rules -> Exp -> Exp
eval _ e@(VarExp _ _)  = e
eval r (ConExp t c as) = ConExp t c $ map (eval r) as
eval r (AppExp t f as) = case lookup f r of
    Nothing -> error "Did you type-check?"
    Just rf -> case rights $ map (contract con) rf of
        (e':_) -> eval r e'
        _    -> es
  where
    as' = map (eval r) as
    es  = AppExp t f as'
    con = EApp t as'
eval r (DesExp t n args inner) = case reducible es of     -- TODO factor out.
    Left _         -> error "Did you type-check?"
    Right (con, f) -> case lookup f r of
        Nothing -> error "Did you type-check?"
        Just rf -> case rights $ map (contract con) rf of
            (e':_) -> eval r e'
            _    -> es
  where
    args'  = map (eval r) args
    inner' = eval r inner
    es     = DesExp t n args' inner'
