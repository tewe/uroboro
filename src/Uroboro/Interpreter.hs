module Uroboro.Interpreter where

import Control.Monad (zipWithM)

type Identifier = String
type Type = Identifier

data T = TVar Type Identifier
       | TApp Type Identifier [T]
       | TCon Type Identifier [T]
       | TDes Type Identifier [T] T deriving (Show, Eq)

data P = PVar Type Identifier
       | PCon Type Identifier [P] deriving (Show, Eq)

data Q = QApp Type [P]
       | QDes Type Identifier [P] Q deriving (Show, Eq)

type Rule = (Q, T)
type Rules = [(Identifier, [Rule])]

type Signature = ([Type], Type)
type Sigma = [(Identifier, Signature)]

data E = EApp Type [T]
       | EDes Type Identifier [T] E deriving (Show, Eq)

type Substitution = [(Identifier, T)]

-- |Pattern matching
pmatch :: T -> P -> Either String Substitution
pmatch (TVar _ _) _          = error "Substitute variables before trying to match"
pmatch term (PVar r x)
    | returnType term /= r   = Left "Type Mismatch"
    | otherwise              = return [(x, term)]
  where
    returnType (TVar t _)     = t
    returnType (TApp t _ _)   = t
    returnType (TCon t _ _)   = t
    returnType (TDes t _ _ _) = t
pmatch (TCon r c ts) (PCon r' c' ps)
    | r /= r'                = Left "Type Mismatch"
    | c /= c'                = Left "Name Mismatch"
    | length ts /= length ps = Left "Argument Length Mismatch"
    | otherwise              = zipWithM pmatch ts ps >>= return . concat
pmatch _ _                   = Left "Not Comparable"

-- |Copattern matching
qmatch :: E -> Q -> Either String Substitution
qmatch (EApp r as) (QApp r' ps)
    | r /= r'                = error "Type checker guarantees hole type"
    | length as /= length ps = Left "Argument Length Mismatch"
    | otherwise              = zipWithM pmatch as ps >>= return . concat -- TODO disjoint
qmatch (EDes r d as inner) (QDes r' d' ps inner')
    | r /= r'                = Left "Type Mismatch"
    | d /= d'                = Left "Name Mismatch"
    | length as /= length ps = Left "Argument Length Mismatch"
    | otherwise              = do
        is <- qmatch inner inner'
        ss <- zipWithM pmatch as ps
        return $ is ++ (concat ss)
qmatch _ _                   = Left "Not Comparable"
