module Uroboro.Tree where

data PExp = PVar String
          | PApp String [PExp]
          | PDes PExp String [PExp] deriving (Eq, Show)
