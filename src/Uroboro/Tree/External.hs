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
       , PExp (PVar, PApp, PDes)
       , PP (PPVar, PPCon)
       , PQ (PQApp, PQDes)
       , PTCon (PTCon)
       , PTDes (PTDes)
       , PTRule (PTRule)
       , PT (PTPos, PTNeg, PTFun)
       ) where

import Uroboro.Error (Location)
import Uroboro.Tree.Common

-- $common
-- Reexported from "Uroboro.Tree.Common".

-- |Expression (Term).
data PExp
    -- |Variable.
    = PVar Location Identifier
    -- |Constructor or function application.
    | PApp Location Identifier [PExp]
    -- |Destructor application (Selection).
    | PDes Location Identifier [PExp] PExp deriving (Show)

-- |Pattern.
data PP
    -- |Variable pattern.
    = PPVar Location Identifier
    -- |Constructor pattern.
    | PPCon Location Identifier [PP] deriving (Show)

-- |Copattern.
data PQ
    -- |Hole pattern.
    = PQApp Location Identifier [PP]
    -- |Destructor pattern.
    | PQDes Location Identifier [PP] PQ deriving (Show)

-- |Constructor definition.
data PTCon = PTCon Location Type Identifier [Type] deriving (Show)

-- |Destructor definition.
-- Return type first, type to destruct last.
data PTDes = PTDes Location Type Identifier [Type] Type deriving (Show)

-- |Part of a function definition.
data PTRule = PTRule Location PQ PExp deriving (Show)

-- |Definition.
data PT
    -- |Data type.
    = PTPos Location Type [PTCon]
    -- |Codata type.
    | PTNeg Location Type [PTDes]
    -- |Function.
    | PTFun Location Identifier [Type] Type [PTRule] deriving (Show)

