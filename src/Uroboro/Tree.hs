module Uroboro.Tree where

type Identifier = String
type Type = Identifier

{- Parse Tree -}

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

{- Syntax Tree -}

data SFun = SFun Identifier [Type] Type deriving (Eq, Show)

type Signature = ([Type], Type)

type Sigma = [(Identifier, Signature)]

type Rule = (PQ, PExp)

type Rules = [(Identifier, [Rule])]

type SLib = ([PTCon], [PTDes], [SFun], [(PQ, PExp)])

{- Typed Syntax Tree -}

data TExp = TVar Type Identifier
          | TApp Type Identifier [TExp]
          | TCon Type Identifier [TExp]
          | TDes Type Identifier [TExp] TExp deriving (Show, Eq)

data TP = TPVar Type Identifier
        | TPCon Type Identifier [TP] deriving (Show, Eq)

data TQ = TQApp Type Identifier [TP]
        | TQDes Type Identifier [TP] TQ deriving (Show, Eq)
