{-|
Description : Parse tree and AST

The program representations we work on.
-}
module Uroboro.Tree where

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
    = PVar Identifier
    -- |Constructor or function application.
    | PApp Identifier [PExp]
    -- |Destructor application (Selection).
    | PDes Identifier [PExp] PExp deriving (Eq, Show)

-- |Pattern.
data PP
    -- |Variable pattern.
    = PPVar Identifier
    -- |Constructor pattern.
    | PPCon Identifier [PP] deriving (Eq, Show)

-- |Copattern.
data PQ
    -- |Hole pattern.
    = PQApp Identifier [PP]
    -- |Destructor pattern.
    | PQDes Identifier [PP] PQ deriving (Eq, Show)

-- |Constructor definition.
data PTCon = PTCon Type Identifier [Type] deriving (Eq, Show)

-- |Destructor definition.
-- Return type first, type to destruct last.
data PTDes = PTDes Type Identifier [Type] Type deriving (Eq, Show)

-- |Part of a function definition.
data PTRule = PTRule PQ PExp deriving (Eq, Show)

-- |Definition.
data PT
    -- |Data type.
    = PTPos Type [PTCon]
    -- |Codata type.
    | PTNeg Type [PTDes]
    -- |Function.
    | PTFun Identifier [Type] Type [PTRule] deriving (Eq, Show)

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
