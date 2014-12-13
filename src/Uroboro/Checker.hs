module Uroboro.Checker where

import Control.Monad (foldM, zipWithM)
import Data.List (find, intercalate, (\\))

import Uroboro.Tree

type PTSig = (Identifier, ([Type], Type))

data Program = Program {
      typeNames    :: [Type]  -- Cache types from constructors and destructors.
    , constructors :: [PTCon]
    , destructors  :: [PTDes]
    , functions    :: [PTSig] -- Always update functions and rules together.
    , rules        :: Rules
    } deriving (Eq, Show)

emptyProgram :: Program
emptyProgram = Program [] [] [] [] []

type Context = [(Identifier, Type)]

texpReturnType :: TExp -> Type
texpReturnType (TVar t _) = t
texpReturnType (TApp t _ _) = t
texpReturnType (TCon t _ _) = t
texpReturnType (TDes t _ _ _) = t

-- |Typecheck a term
checkPExp :: Program -> Context -> PExp -> Type -> Either String TExp
checkPExp p c e t = Left "TODO"

-- |Infer a term's type
inferPExp :: Program -> Context -> PExp -> Either String TExp
inferPExp _ context (PVar name) = case lookup name context of
    Nothing  -> Left "Unbound Variable"
    Just typ -> Right (TVar typ name)
inferPExp p c (PApp name args) = case lookup name (functions p) of  -- TODO catch ambiguous
    Just (argTypes, returnType) ->
        zipWithM (checkPExp p c) args argTypes >>= return . TApp returnType name
    Nothing -> case find match (constructors p) of
        Just (PTCon returnType _ argTypes) ->
            zipWithM (checkPExp p c) args argTypes >>= return . TCon returnType name
        Nothing -> Left "Missing Definition"
  where
    match (PTCon _ n _) = n == name
inferPExp p c (PDes name args inner) = do
    tinner <- inferPExp p c inner
    (argTypes, returnType) <- nu p (texpReturnType tinner) name
    targs <- zipWithM (checkPExp p c) args argTypes
    return $ TDes returnType name targs tinner
  where
    -- |Look up signature of named destructor for type
    nu :: Program -> Type -> Identifier -> Either String ([Type], Type)
    nu p innerType name = case find match (destructors p) of
        Just (PTDes returnType _ argTypes _) -> return (argTypes, returnType)
        Nothing -> Left "Missing Definition"
      where
        match (PTDes _ n _ t) = n == name && t == innerType

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
