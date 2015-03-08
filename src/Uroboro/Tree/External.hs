{-|
Description : Parse tree.

Representation of Uroboro programs as abstract syntax tree. This
is the "external" program representation produced by the parser.
-}

module Uroboro.Tree.External
       ( -- * Common parts
         -- $common
         Identifier
       , Type (Type)
         -- * Parse tree
       , Exp (VarExp, AppExp, DesExp)
       , Pat (VarPat, ConPat)
       , Cop (AppCop, DesCop)
       , ConSig (ConSig)
       , DesSig (DesSig)
       , Rule (Rule)
       , Def (DatDef, CodDef, FunDef)
       ) where

import Uroboro.Error (Location)
import Uroboro.Tree.Common

-- $common
-- Reexported from "Uroboro.Tree.Common".

-- |Expression (Term).
data Exp
    -- |Variable.
    = VarExp Location Identifier
    -- |Constructor or function application.
    | AppExp Location Identifier [Exp]
    -- |Destructor application (Selection).
    | DesExp Location Identifier [Exp] Exp deriving (Show)

-- |Pattern.
data Pat
    -- |Variable pattern.
    = VarPat Location Identifier
    -- |Constructor pattern.
    | ConPat Location Identifier [Pat] deriving (Show)

-- |Copattern.
data Cop
    -- |Hole pattern.
    = AppCop Location Identifier [Pat]
    -- |Destructor pattern.
    | DesCop Location Identifier [Pat] Cop deriving (Show)

-- |Constructor definition.
data ConSig = ConSig Location Type Identifier [Type] deriving (Show)

-- |Destructor definition.
-- Return type first, type to destruct last.
data DesSig = DesSig Location Type Identifier [Type] Type deriving (Show)

-- |Part of a function definition.
data Rule = Rule Location Cop Exp deriving (Show)

-- |Definition.
data Def
    -- |Data type.
    = DatDef Location Type [ConSig]
    -- |Codata type.
    | CodDef Location Type [DesSig]
    -- |Function.
    | FunDef Location Identifier [Type] Type [Rule] deriving (Show)

