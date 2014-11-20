module Uroboro.Tree where

data PExp = PVar String
          | PApp String [PExp]
          | PDes String [PExp] PExp deriving (Eq, Show)
