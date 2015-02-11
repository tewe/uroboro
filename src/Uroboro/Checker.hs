{-|
Description : Typechecker

Typecheck parser output, which turns it into interpreter input.
-}
module Uroboro.Checker
    (
      checkPExp
    , preCheckPT
    , postCheckPT
    , checkPT
    , Context
    , emptyProgram
    , inferPExp
    , Program
    , rules
    , typecheck
    ) where

import Control.Monad (foldM, zipWithM)
import Data.List ((\\), find, nub, nubBy)

import Uroboro.Error (Error, Location, failAt)

import Uroboro.Tree
    (
      Identifier
    , PExp(..)
    , PP(..)
    , PQ(..)
    , PT(..)
    , PTCon(..)
    , PTDes(..)
    , PTRule(..)
    , Rule
    , Rules
    , TExp(..)
    , TP(..)
    , TQ(..)
    , Type(..)
    )

-- |Signature of a function definition.
type PTSig = (Identifier, (Location, [Type], Type))

-- |State of the typechecker.
data Program = Program {
      typeNames    :: [Type]  -- Cache types of constructors and destructors.
    , constructors :: [PTCon]
    , destructors  :: [PTDes]
    , functions    :: [PTSig] -- Always update functions and rules together.
    -- |Extract the end result of typechecking.
    , rules        :: Rules
    } deriving (Show)

-- |Start value for folds.
emptyProgram :: Program
emptyProgram = Program [] [] [] [] []

-- |Types of the variables bound in a pattern.
type Context = [(Identifier, Type)]

-- |Only keep the first type for each identifier.
nubContext :: Context -> Context
nubContext c = nubBy f c
  where
    f (a, _) (b, _) = a == b

-- |Extract variable types from a typed pattern.
tpContext :: TP -> Context
tpContext (TPVar t n) = [(n, t)]
tpContext (TPCon _ _ args) = concat $ map tpContext args

-- |Extract variable types from a typed copattern.
tqContext :: TQ -> Context
tqContext (TQApp _ _ args) = concat $ map tpContext args
tqContext (TQDes _ _ args inner) = concat [tqContext inner, concat $ map tpContext args]

-- |A zipWithM that requires identical lengths.
zipStrict :: Location -> Location -> (a -> b -> Either Error c) -> [a] -> [b] -> Either Error [c]
zipStrict loc _loc' f a b
  | length a == length b = zipWithM f a b
  | otherwise            = failAt loc "Length Mismatch"

-- |Typecheck a pattern
checkPP :: Program -> PP -> Type -> Either Error TP
checkPP _ (PPVar _loc name) t = return (TPVar t name)
checkPP p (PPCon loc name args) t = case find match (constructors p) of
    Just (PTCon loc' _ _ argTypes) ->
        zipStrict loc loc' (checkPP p) args argTypes >>= return . TPCon t name
    Nothing -> failAt loc "Missing Definition"
  where
    match (PTCon _loc' returnType n _) = n == name && returnType == t

-- |Typecheck a copattern. Takes hole type.
checkPQ :: Program -> PQ -> PTSig -> Either Error TQ
checkPQ p (PQApp loc name args) (name', (loc', argTypes, returnType))
    | name == name' = do
        targs <- zipStrict loc loc' (checkPP p) args argTypes
        return $ TQApp returnType name targs
    | otherwise     = failAt loc "Definition Mismatch"
checkPQ p (PQDes loc name args inner) s = do
    tinner <- checkPQ p inner s
    case find (match (tqReturnType tinner)) (destructors p) of
        Nothing -> failAt loc $
            "Missing Definition: " ++ (typeName $ tqReturnType tinner) ++ "." ++ name
        Just (PTDes loc' returnType _ argTypes _) -> do
            targs <- zipStrict loc loc' (checkPP p) args argTypes
            return $ TQDes returnType name targs tinner
  where
    match t (PTDes _loc' _ n _ innerType) = n == name && innerType == t

-- |The type a copattern matches.
tqReturnType :: TQ -> Type
tqReturnType (TQApp t _ _) = t
tqReturnType (TQDes t _ _ _) = t

-- |Typecheck a term.
checkPExp :: Program -> Context -> PExp -> Type -> Either Error TExp
checkPExp _ c (PVar loc n) t = case lookup n c of
    Just t' | t' == t   -> return (TVar t n)
            | otherwise -> failAt loc $ "Type Mismatch: " ++ n ++
                " expected to be " ++ typeName t ++ " but is actually " ++ typeName t'
    Nothing             -> failAt loc $ "Unbound Variable: " ++ n
checkPExp p c (PApp loc name args) t = case lookup name (functions p) of
    Just (loc', argTypes, returnType) | returnType == t ->
                zipStrict loc loc' (checkPExp p c) args argTypes >>= return . TApp returnType name
        | otherwise -> failAt loc "Type Mismatch"
    Nothing -> case find match (constructors p) of
        Just (PTCon loc' _ _ argTypes) ->
            zipStrict loc loc'  (checkPExp p c) args argTypes >>= return . TCon t name
        Nothing -> failAt loc "Missing Definition"
  where
    match (PTCon _loc' returnType n _) = n == name && returnType == t
checkPExp p c (PDes loc name args inner) t = case find match (destructors p) of
    Nothing -> failAt loc $
        "Missing Definition: no destructor to get " ++ typeName t ++ " from " ++ name
    Just (PTDes loc' _ _ argTypes innerType) -> do
        tinner <- checkPExp p c inner innerType
        targs <- zipStrict loc loc' (checkPExp p c) args argTypes
        return $ TDes t name targs tinner
  where
    match (PTDes _loc' r n a _) = n == name && r == t && length a == length args

-- |Infer the type of a term.
inferPExp :: Program -> Context -> PExp -> Either Error TExp
inferPExp _ context (PVar loc name) = case lookup name context of
    Nothing  -> failAt loc "Unbound Variable"
    Just typ -> Right (TVar typ name)
inferPExp p c (PApp loc name args) = case lookup name (functions p) of
    Just (loc', argTypes, returnType) ->
        zipStrict loc loc' (checkPExp p c) args argTypes >>= return . TApp returnType name
    Nothing -> case find match (constructors p) of
        Just (PTCon loc' returnType _ argTypes) ->
            zipStrict loc loc' (checkPExp p c) args argTypes >>= return . TCon returnType name
        Nothing -> failAt loc "Missing Definition"
  where
    match (PTCon _loc' _ n _) = n == name
inferPExp p c (PDes loc name args inner) = do
    tinner <- inferPExp p c inner
    case find (match (texpReturnType tinner)) (destructors p) of
        Nothing -> failAt loc "Missing Definition"
        Just (PTDes loc' returnType _ argTypes _) -> do
            targs <- zipStrict loc loc' (checkPExp p c) args argTypes
            return $ TDes returnType name targs tinner
  where
    texpReturnType :: TExp -> Type
    texpReturnType (TVar t _) = t
    texpReturnType (TApp t _ _) = t
    texpReturnType (TCon t _ _) = t
    texpReturnType (TDes t _ _ _) = t

    match t' (PTDes _loc' _ n _ t) = n == name && t == t'

-- |Identify a type to the user.
typeName :: Type -> Identifier
typeName (Type n) = n

-- |Typecheck a rule against the function's signature.
checkPTRule :: Program -> PTSig -> PTRule -> Either Error Rule
checkPTRule p s (PTRule loc left right) = do
    tleft <- checkPQ p left s
    let c = tqContext tleft
    tright <- checkPExp p c right (tqReturnType tleft)
    let d = nubContext c
    if length c == length d then
        return (tleft, tright)
    else
        failAt loc "Shadowed Variable"

-- |Fold to collect definitions.
preCheckPT :: Program -> PT -> Either Error Program
preCheckPT prog@(Program names cons _ _ _) (PTPos loc name cons')
    | name `elem` names  = failAt loc "Shadowed Definition"
    | any mismatch cons' = failAt loc "Definition Mismatch"
    | otherwise          = Right prog {
          typeNames = (name:names)
        , constructors = cons ++ cons'
        }
  where
    mismatch (PTCon _loc' returnType _ _) = returnType /= name
preCheckPT prog@(Program names _ des _ _) (PTNeg loc name des')
    | name `elem` names = failAt loc "Shadowed Definition"
    | any mismatch des' = failAt loc "Definition Mismatch"
    | otherwise         = Right prog {
          typeNames = (name:names)
        , destructors = des ++ des'
        }
  where
    mismatch (PTDes _loc' _ _ _ innerType) = innerType /= name
preCheckPT prog@(Program _ _ _ funs rulz) (PTFun loc name argTypes returnType _)
    | any clash rulz     = failAt loc "Shadowed Definition"
    | otherwise = do
        let sig = (name, (loc, argTypes, returnType))
        let recursive = prog {
              functions = (sig:funs)
            }
        return recursive
  where
    clash (name', _) = name' == name

-- |Fold to typecheck definitions.
postCheckPT :: Program -> PT -> Either Error Program
postCheckPT prog@(Program names _ _ _ _) (PTPos loc name cons')
    | any missing cons'  = failAt loc "Missing Definition"
    | otherwise          = Right prog
  where
    missing (PTCon _loc' _ _ args)        = (nub args) \\ (name:names) /= []
postCheckPT prog@(Program names _ _ _ _) (PTNeg loc name des')
    | any missing des'  = failAt loc $
        "Missing Definition: " ++ typeName name ++
        " has a destructor with an unknown argument type"
    | otherwise         = Right prog
  where
    missing (PTDes _loc' _ _ args _)       = (nub args) \\ (name:names) /= []
postCheckPT prog@(Program _ _ _ _ rulz) (PTFun loc name argTypes returnType rs)
    = do
        let sig = (name, (loc, argTypes, returnType))
        trs <- mapM (checkPTRule prog sig) rs
        return prog {
              rules = ((name, trs):rulz)
            }

-- |Fold to typecheck definitions.
checkPT :: Program -> PT -> Either Error Program
checkPT prog def = preCheckPT prog def >>= (flip postCheckPT) def

-- |Turn parser output into interpreter input.
typecheck :: [PT] -> Either Error Rules
typecheck defs = do
    pre  <- foldM preCheckPT emptyProgram defs
    prog <- foldM postCheckPT pre defs
    return $ rules prog
