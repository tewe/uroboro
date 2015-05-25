{-|
Description : Common parts of trees.

Reusable parts of the various representations of Uroboro programs
as syntax trees.
-}

module Uroboro.Tree.Common
       ( Identifier
       , Type (Type)
       ) where

-- |This is used for type names, function names, constructor and
-- destructor names, as well as variable names.
type Identifier = String

-- |Represents both positive and negative data types.
newtype Type = Type Identifier deriving (Eq, Show)
