{-|
Description : Abstract syntax tree.

Representation of Uroboro programs as abstract syntax tree with
type annotations. This is the "internal" program representation
produced by the type checker.
-}

module Uroboro.Tree.Internal
       ( -- * Common parts
         -- $common
         Identifier
       , Type (Type)
         -- * Typed syntax tree
       , TExp (TVar, TApp, TCon, TDes)
       , TP (TPVar, TPCon)
       , TQ (TQApp, TQDes)
       , Rule
       , Rules
       ) where

import Uroboro.Tree.Common

-- $common
-- Reexported from "Uroboro.Tree.Common".

-- |Expression with type annotations.
data TExp
    -- |Variable.
    = TVar Type Identifier
    -- |Function application.
    | TApp Type Identifier [TExp]
    -- |Constructor application.
    | TCon Type Identifier [TExp]
    -- |Destructor application.
    | TDes Type Identifier [TExp] TExp deriving (Show, Eq)

-- |Pattern with type annotations.
data TP = TPVar Type Identifier
        | TPCon Type Identifier [TP] deriving (Show, Eq)

-- |Copattern with type annotations.
data TQ = TQApp Type Identifier [TP]
        | TQDes Type Identifier [TP] TQ deriving (Show, Eq)

-- |One rule of a function definition.
type Rule = (TQ, TExp)

-- |A complete program.
type Rules = [(Identifier, [Rule])]
