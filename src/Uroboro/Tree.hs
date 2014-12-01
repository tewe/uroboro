module Uroboro.Tree where

type Identifier = String
type Type = Identifier
data PExp = PVar Identifier
          | PApp Identifier [PExp]
          | PDes Identifier [PExp] PExp deriving (Eq, Show)

data PP = PPVar Identifier
        | PPCon Identifier [PP] deriving (Eq, Show)

data PQ = PQApp Identifier [PP]
        | PQDes Identifier [PP] PQ deriving (Eq, Show)

data PTCon = PTCon Identifier [Type] Type deriving (Eq, Show)

data PTDes = PTDes Type Identifier [Type] Type deriving (Eq, Show)

data PTRule = PTRule PQ PExp deriving (Eq, Show)

data PT = PTPos Type [PTCon]
        | PTNeg Type [PTDes]
        | PTFun Identifier [Type] Type [PTRule] deriving (Eq, Show)
