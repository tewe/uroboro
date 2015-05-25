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
       , Exp (VarExp, AppExp, ConExp, DesExp)
       , Pat (VarPat, ConPat)
       , Cop (AppCop, DesCop)
       , Rule
       , Rules
       ) where

import Uroboro.Tree.Common

-- $common
-- Reexported from "Uroboro.Tree.Common".

-- |Expression with type annotations.
data Exp
    -- |Variable.
    = VarExp Type Identifier
    -- |Function application.
    | AppExp Type Identifier [Exp]
    -- |Constructor application.
    | ConExp Type Identifier [Exp]
    -- |Destructor application.
    | DesExp Type Identifier [Exp] Exp deriving (Show, Eq)

-- |Pattern with type annotations.
data Pat = VarPat Type Identifier
         | ConPat Type Identifier [Pat] deriving (Show, Eq)

-- |Copattern with type annotations.
data Cop = AppCop Type Identifier [Pat]
         | DesCop Type Identifier [Pat] Cop deriving (Show, Eq)

-- |One rule of a function definition.
type Rule = (Cop, Exp)

-- |A complete program.
type Rules = [(Identifier, [Rule])]
