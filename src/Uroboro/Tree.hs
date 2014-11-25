module Uroboro.Tree where

data PExp = PVar String
          | PApp String [PExp]
          | PDes String [PExp] PExp deriving (Eq, Show)

data PP = PPVar String
        | PPCon String [PP] deriving (Eq, Show)

data PQ = PQApp String [PP]
        | PQDes String [PP] PQ deriving (Eq, Show)

data PPos = PPos String [String] String deriving (Eq, Show)

data PNeg = PNeg String String [String] String deriving (Eq, Show)
