module Uroboro.Interpreter where

import Control.Monad (zipWithM)
import Data.Either (isRight)

import Uroboro.Tree

data E = EApp Type [TExp]
       | EDes Type Identifier [TExp] E deriving (Show, Eq)

type Substitution = [(Identifier, TExp)]

-- |Pattern matching
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

-- |Copattern matching (inside-out?)
qmatch :: E -> TQ -> Either String Substitution
qmatch (EApp r as) (TQApp r' _ ps)
    | r /= r'                = error "Type checker guarantees hole type"
    | length as /= length ps = Left "Argument Length Mismatch"
    | otherwise              = zipWithM pmatch as ps >>= return . concat -- TODO disjoint
qmatch (EDes r d as inner) (TQDes r' d' ps inner')
    | r /= r'                = Left "Type Mismatch"
    | d /= d'                = Left "Name Mismatch"
    | length as /= length ps = Left "Argument Length Mismatch"
    | otherwise              = do
        is <- qmatch inner inner'
        ss <- zipWithM pmatch as ps
        return $ is ++ (concat ss)
qmatch _ _                   = Left "Not Comparable"

-- |Substitute
subst :: TExp -> (Identifier, TExp) -> TExp
subst t@(TVar _ n') (n, term)
    | n == n'               = term
    | otherwise             = t
subst (TApp t n as) s       = TApp t n $ map (flip subst s) as
subst (TCon t n as) s       = TCon t n $ map (flip subst s) as
subst (TDes t n as inner) s = TDes t n (map (flip subst s) as) (subst inner s)

-- |Contraction
contract :: E -> Rule -> Either String TExp
contract context (pattern, term) = do
    s <- qmatch context pattern
    return $ foldl subst term s

-- | Tag the 'Nothing' value of a 'Maybe'
-- http://hackage.haskell.org/package/errors-1.2.1/docs/src/Control-Error-Util.html#note
note :: a -> Maybe b -> Either a b
note a m = case m of
    Nothing -> Left  a
    Just b  -> Right b

-- |Find hole
reducible :: TExp -> Either String (E, Identifier)  -- Could be Maybe.
reducible (TApp r f args) = return (EApp r args, f)
reducible (TDes r d args inner) = do
    (inner', f) <- reducible inner
    return (EDes r d args inner', f)
reducible t = Left $ "Not a redex: " ++ show t

-- |Reduce leftmost reducible argument.
red :: Rules -> [TExp] -> Either String [TExp]
red _ [] = Left "Not a redex"
red r (x:xs) = case reduce r x of
    Left _ -> do
        xs' <- red r xs
        return (x:xs')
    Right x' -> do
        return (x':xs)

-- |One step reduction
reduce :: Rules -> TExp -> Either String TExp
reduce r (TCon t n args) = red r args >>= return . TCon t n
reduce rules term = do
    (context, f) <- reducible term
    rulesf <- note ("Missing Definition: " ++ f) $ lookup f rules
    let ts = map (contract context) rulesf
    case filter isRight ts of
        [Right t'] -> return t'
        _ -> Left $ "Multiple Matches: " ++ show term

-- |Star reduction.
eval :: Rules -> TExp -> TExp
eval r e = case reduce r e of
    Left _ -> e
    Right e' -> eval r e'
