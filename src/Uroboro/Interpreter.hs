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

import Uroboro.Tree
    (
      Identifier
    , Rule
    , Rules
    , TExp(..)
    , TP(..)
    , TQ(..)
    , Type(..)
    )

-- |Evaluation contexts.
data E = EApp Type [TExp]
       | EDes Type Identifier [TExp] E deriving (Show, Eq)

-- |Result of a pattern match.
type Substitution = [(Identifier, TExp)]

-- |Pattern matching.
pmatch :: TExp -> TP -> Either String Substitution
pmatch (TVar _ _) _          = error "Substitute variables before trying to match"
pmatch term (TPVar r x)
    | returnType term /= r   = Left "Type Mismatch"
    | otherwise              = return [(x, term)]
  where
    returnType (TVar t _)     = t
    returnType (TApp t _ _)   = t
    returnType (TCon t _ _)   = t
    returnType (TDes t _ _ _) = t
pmatch (TCon r c ts) (TPCon r' c' ps)
    | r /= r'                = Left "Type Mismatch"
    | c /= c'                = Left $
        "Name Mismatch: constructor " ++ c' ++ " doesn't match pattern " ++ c
    | length ts /= length ps = Left "Argument Length Mismatch"
    | otherwise              = zipWithM pmatch ts ps >>= return . concat
pmatch _ _                   = Left "Not Comparable"

-- |Copattern matching.
qmatch :: E -> TQ -> Either String Substitution
qmatch (EApp r as) (TQApp r' _ ps)
    | r /= r'                = error "Type checker guarantees hole type"
    | length as /= length ps = Left "Argument Length Mismatch"
    | otherwise              = zipWithM pmatch as ps >>= return . concat
qmatch (EDes r d as inner) (TQDes r' d' ps inner')
    | r /= r'                = Left "Type Mismatch"
    | d /= d'                = Left "Name Mismatch"
    | length as /= length ps = Left "Argument Length Mismatch"
    | otherwise              = do
        is <- qmatch inner inner'
        ss <- zipWithM pmatch as ps
        return $ is ++ (concat ss)
qmatch _ _                   = Left "Not Comparable"

-- |Substitute all occurences.
subst :: TExp -> (Identifier, TExp) -> TExp
subst t@(TVar _ n') (n, term)
    | n == n'               = term
    | otherwise             = t
subst (TApp t n as) s       = TApp t n $ map (flip subst s) as
subst (TCon t n as) s       = TCon t n $ map (flip subst s) as
subst (TDes t n as inner) s = TDes t n (map (flip subst s) as) (subst inner s)

-- |If context matches rule, apply it.
contract :: E -> Rule -> Either String TExp
contract context (pattern, term) = do
    s <- qmatch context pattern
    return $ foldl subst term s

-- |Find hole.
reducible :: TExp -> Either String (E, Identifier)  -- Could be Maybe.
reducible (TApp r f args) = return (EApp r args, f)
reducible (TDes r d args inner) = do
    (inner', f) <- reducible inner
    return (EDes r d args inner', f)
reducible t = Left $ "Not a redex: " ++ show t

-- |Star reduction.
eval :: Rules -> TExp -> TExp
eval _ e@(TVar _ _)  = e
eval r (TCon t c as) = TCon t c $ map (eval r) as
eval r (TApp t f as) = case lookup f r of
    Nothing -> error "Did you type-check?"
    Just rf -> case rights $ map (contract con) rf of
        (e':_) -> eval r e'
        _    -> es
  where
    as' = map (eval r) as
    es  = TApp t f as'
    con = EApp t as'
eval r (TDes t n args inner) = case reducible es of     -- TODO factor out.
    Left _         -> error "Did you type-check?"
    Right (con, f) -> case lookup f r of
        Nothing -> error "Did you type-check?"
        Just rf -> case rights $ map (contract con) rf of
            (e':_) -> eval r e'
            _    -> es
  where
    args'  = map (eval r) args
    inner' = eval r inner
    es     = TDes t n args' inner'
