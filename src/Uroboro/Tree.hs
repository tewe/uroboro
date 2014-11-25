module Uroboro.Tree where

data PExp = PVar String
          | PApp String [PExp]
          | PDes String [PExp] PExp deriving (Eq, Show)

data PP = PPVar String
        | PPCon String [PP] deriving (Eq, Show)

data PQ = PQApp String [PP]
        | PQDes String [PP] PQ deriving (Eq, Show)

data PTCon = PTCon String [String] String deriving (Eq, Show)

data PTDes = PTDes String String [String] String deriving (Eq, Show)

data PTRule = PTRule PQ PExp deriving (Eq, Show)

data PT = PTPos String [PTCon]
        | PTNeg String [PTDes]
        | PTFun String [String] String [PTRule] deriving (Eq, Show)