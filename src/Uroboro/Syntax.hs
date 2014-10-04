module Uroboro.Syntax
    (
      Exp(..)
    , Pattern(..)
    ) where

type Identifier = String

data Exp = Variable Identifier
         | Application Identifier [Exp]
--       | FunctionApplication Identifier [Exp]
--       | ConstructorApplication Identifier [Exp]
         | DestructorApplication Exp Identifier [Exp] deriving (Show, Eq)

data Pattern = VariablePattern Identifier
             | ConstructorPattern Identifier [Pattern] deriving (Show, Eq)
