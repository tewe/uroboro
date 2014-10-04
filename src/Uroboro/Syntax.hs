module Uroboro.Syntax
    (
      Exp(..)
    ) where

type Identifier = String

data Exp = Variable Identifier
         | Application Identifier [Exp]
--       | FunctionApplication Identifier [Exp]
--       | ConstructorApplication Identifier [Exp]
         | DestructorApplication Exp Identifier [Exp] deriving (Show, Eq)
