module Uroboro.Syntax
    (
      Exp(..)
    , Pattern(..)
    , Signature(..)
    , Definition(..)
    ) where

type Identifier = String

data Exp = Variable Identifier
         | Application Identifier [Exp]
--       | FunctionApplication Identifier [Exp]
--       | ConstructorApplication Identifier [Exp]
         | DestructorApplication Exp Identifier [Exp] deriving (Show, Eq)

data Pattern = VariablePattern Identifier
             | ConstructorPattern Identifier [Pattern] deriving (Show, Eq)

-- data Type = PositiveType Identifier
--           | NegativeType Identifier
type Type = Identifier

data Signature = Signature Identifier [Type] Type deriving (Show, Eq)

data Definition = DataDefinition Identifier [Signature] deriving (Show, Eq)
