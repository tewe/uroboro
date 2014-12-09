module Uroboro.Checker where

import Control.Monad (foldM)
import Data.List (find, intercalate, (\\))

import Uroboro.Tree

data PTSig = PTSig Identifier [Type] Type deriving (Eq, Show)

data Program = Program {
      typeNames    :: [Type]  -- Cache types from constructors and destructors.
    , constructors :: [PTCon]
    , destructors  :: [PTDes]
    , functions    :: [PTSig] -- Always update functions and rules together.
    , rules        :: Rules
    }

emptyProgram :: Program
emptyProgram = Program [] [] [] [] []

type Context = [(Identifier, Type)]

inferPExp :: Program -> Context -> PExp -> Either String TExp
inferPExp _ context (PVar name) = case lookup name context of
    Nothing  -> Left "Unbound Variable"
    Just typ -> Right (TVar typ name)
inferPExp p _ (PApp name _) = Left "TODO"
inferPExp _ _ (PDes _ _ _) = Left "TODO"

-- |Fold over type definitions.
checkPT :: Program -> PT -> Either String Program
checkPT prog@(Program names cons _ _ _) (PTPos name cons')
    | name `elem` names  = Left "Shadowed Definition"
    | any mismatch cons' = Left "Definition Mismatch"
    | any missing cons'  = Left "Missing Definition"
    | otherwise          = Right prog {
          typeNames = (name:names)
        , constructors = cons ++ cons'
        }
  where
    mismatch (PTCon returnType _ _) = returnType /= name
    missing (PTCon _ _ args)        = args \\ (name:names) /= []
checkPT prog@(Program names _ des _ _) (PTNeg name des')
    | name `elem` names = Left "Shadowed Definition"
    | any mismatch des' = Left "Definition Mismatch"
    | any missing des'  = Left "Missing Definition"
    | otherwise         = Right prog {
          typeNames = (name:names)
        , destructors = des ++ des'
        }
  where
    mismatch (PTDes _ _ _ innerType) = innerType /= name
    missing (PTDes _ _ args _)       = args \\ (name:names) /= []
checkPT prog@(Program names _ _ funs rules) (PTFun name args t rules')
    | any clash rules     = Left "Shadowed Definition"
  where
    clash (name', _) = name' == name

-- |Turn parser output into interpreter input.
typecheck :: [PT] -> Either String Rules
typecheck defs = foldM checkPT emptyProgram defs >>= return . rules

-- |Infer a term's type
expInfer :: [PT] -> Context -> PExp -> Either String TExp
expInfer _ vars (PVar name) = case lookup name vars of
    Just t  -> return (TVar t name)
    Nothing -> Left $ "Unknown Variable: " ++ name ++ " isn't bound"
expInfer defs vars (PApp name args) = Left "TODO"
expInfer defs vars (PDes name args inner) = do
    fromType <- expInfer defs vars inner
    Left "TODO"

-- |Typecheck a term
exp :: [PT] -> Context -> PExp -> Type -> Either String TExp
exp defs vars (PVar name) returnType = Left "TODO"
exp defs vars (PApp name args) returnType = Left "TODO"
exp defs vars (PDes name args inner) returnType = Left "TODO"
