{-|
Description : Parse tree and AST

The program representations we work on.
-}
module Uroboro.Tree where

import Uroboro.Error (Location)

-- |This is used for type names, function names, constructor and destructor names,
-- as well as variable names.
type Identifier = String
-- |Represents both positive and negative data types.
newtype Type = Type Identifier deriving (Eq, Show)

-- |
-- = Parse Tree

-- TODO move headings into export list.

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

-- |
-- = Typed Syntax Tree

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
