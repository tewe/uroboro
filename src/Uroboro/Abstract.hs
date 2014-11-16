module Uroboro.Abstract where

import Uroboro.Syntax (Identifier, Type)

data TExp = TVar Identifier Type
          | TApp Identifier [TExp] Type
          | TCon Identifier [TExp] Type -- positive
          | TDes Identifier TExp [TExp] Type deriving (Show, Eq) -- negative

data TP = TPVar Identifier Type
        | TPCons Identifier [TP] Type deriving (Show, Eq)

data TQ = TQApp Identifier [TP] Type -- hole
        | TQDes TQ Identifier [TP] Type deriving (Show, Eq)

type TRules = [(TQ, TExp)]
