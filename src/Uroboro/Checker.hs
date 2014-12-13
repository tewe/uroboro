module Uroboro.Checker where

import Control.Monad (foldM, zipWithM)
import Data.List (find, (\\))

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

-- |Typecheck a term
checkPExp :: Program -> Context -> PExp -> Type -> Either String TExp
checkPExp _ c (PVar n) t = case lookup n c of
    Just t' | t' == t   -> return (TVar t n)
            | otherwise -> Left "Type Mismatch"
    Nothing             -> Left "Unbound Variable"
checkPExp p c (PApp name args) t = case lookup name (functions p) of
    Just (argTypes, returnType) | returnType == t ->
                zipWithM (checkPExp p c) args argTypes >>= return . TApp returnType name
        | otherwise -> Left "Type Mismatch"
    Nothing -> case find match (constructors p) of
        Just (PTCon _ _ argTypes) ->
            zipWithM (checkPExp p c) args argTypes >>= return . TCon t name
        Nothing -> Left "Missing Definition"
  where
    match (PTCon returnType n _) = n == name && returnType == t
checkPExp p c (PDes name args inner) t = case find match (destructors p) of
    Nothing -> Left "Missing Definition"
    Just (PTDes _ _ argTypes innerType) -> do
        tinner <- checkPExp p c inner innerType
        targs <- zipWithM (checkPExp p c) args argTypes
        return $ TDes t name targs tinner
  where
    match (PTDes returnType n _ _) = n == name && returnType == t

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
    case find (match (texpReturnType tinner)) (destructors p) of
        Nothing -> Left "Missing Definition"
        Just (PTDes returnType _ argTypes _) -> do
            targs <- zipWithM (checkPExp p c) args argTypes
            return $ TDes returnType name targs tinner
  where
    texpReturnType :: TExp -> Type
    texpReturnType (TVar t _) = t
    texpReturnType (TApp t _ _) = t
    texpReturnType (TCon t _ _) = t
    texpReturnType (TDes t _ _ _) = t

    match t' (PTDes _ n _ t) = n == name && t == t'

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
checkPT p@(Program _ _ _ _ rs) (PTFun name _ _ _)
    | any clash rs     = Left "Shadowed Definition"
    | otherwise = return p  -- TODO
  where
    clash (name', _) = name' == name

-- |Turn parser output into interpreter input.
typecheck :: [PT] -> Either String Rules
typecheck defs = foldM checkPT emptyProgram defs >>= return . rules
