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

import Uroboro.Tree.Common (Identifier)
import qualified Uroboro.Tree.Internal as Int
import qualified Uroboro.Tree.External as Ext

-- |Signature of a function definition.
type PTSig = (Identifier, (Location, [Int.Type], Int.Type))

-- |State of the typechecker.
data Program = Program {
      typeNames    :: [Int.Type]  -- Cache types of constructors and destructors.
    , constructors :: [Ext.PTCon]
    , destructors  :: [Ext.PTDes]
    , functions    :: [PTSig] -- Always update functions and rules together.
    -- |Extract the end result of typechecking.
    , rules        :: Int.Rules
    } deriving (Show)

-- |Start value for folds.
emptyProgram :: Program
emptyProgram = Program [] [] [] [] []

-- |Types of the variables bound in a pattern.
type Context = [(Int.Identifier, Int.Type)]

-- |Only keep the first type for each identifier.
nubContext :: Context -> Context
nubContext c = nubBy f c
  where
    f (a, _) (b, _) = a == b

-- |Extract variable types from a typed pattern.
tpContext :: Int.TP -> Context
tpContext (Int.TPVar t n) = [(n, t)]
tpContext (Int.TPCon _ _ args) = concat $ map tpContext args

-- |Extract variable types from a typed copattern.
tqContext :: Int.TQ -> Context
tqContext (Int.TQApp _ _ args) = concat $ map tpContext args
tqContext (Int.TQDes _ _ args inner) = concat [tqContext inner, concat $ map tpContext args]

-- |A zipWithM that requires identical lengths.
zipStrict :: Location -> Location -> (a -> b -> Either Error c) -> [a] -> [b] -> Either Error [c]
zipStrict loc _loc' f a b
  | length a == length b = zipWithM f a b
  | otherwise            = failAt loc "Length Mismatch"

-- |Typecheck a pattern
checkPP :: Program -> Ext.PP -> Int.Type -> Either Error Int.TP
checkPP _ (Ext.PPVar _loc name) t = return (Int.TPVar t name)
checkPP p (Ext.PPCon loc name args) t = case find match (constructors p) of
    Just (Ext.PTCon loc' _ _ argTypes) ->
        zipStrict loc loc' (checkPP p) args argTypes >>= return . Int.TPCon t name
    Nothing -> failAt loc "Missing Definition"
  where
    match (Ext.PTCon _loc' returnType n _) = n == name && returnType == t

-- |Typecheck a copattern. Takes hole type.
checkPQ :: Program -> Ext.PQ -> PTSig -> Either Error Int.TQ
checkPQ p (Ext.PQApp loc name args) (name', (loc', argTypes, returnType))
    | name == name' = do
        targs <- zipStrict loc loc' (checkPP p) args argTypes
        return $ Int.TQApp returnType name targs
    | otherwise     = failAt loc $
        "Definition Mismatch: " ++ name ++ " used in copattern for " ++ name'
checkPQ p (Ext.PQDes loc name args inner) s = do
    tinner <- checkPQ p inner s
    case find (match (tqReturnType tinner)) (destructors p) of
        Nothing -> failAt loc $
            "Missing Definition: " ++ (typeName $ tqReturnType tinner) ++ "." ++ name
        Just (Ext.PTDes loc' returnType _ argTypes _) -> do
            targs <- zipStrict loc loc' (checkPP p) args argTypes
            return $ Int.TQDes returnType name targs tinner
  where
    match t (Ext.PTDes _loc' _ n _ innerType) = n == name && innerType == t

-- |The type a copattern matches.
tqReturnType :: Int.TQ -> Int.Type
tqReturnType (Int.TQApp t _ _) = t
tqReturnType (Int.TQDes t _ _ _) = t

-- |Typecheck a term.
checkPExp :: Program -> Context -> Ext.PExp -> Int.Type -> Either Error Int.TExp
checkPExp _ c (Ext.PVar loc n) t = case lookup n c of
    Just t' | t' == t   -> return (Int.TVar t n)
            | otherwise -> failAt loc $ "Type Mismatch: " ++ n ++
                " expected to be " ++ typeName t ++ " but is actually " ++ typeName t'
    Nothing             -> failAt loc $ "Unbound Variable: " ++ n
checkPExp p c (Ext.PApp loc name args) t = case lookup name (functions p) of
    Just (loc', argTypes, returnType) | returnType == t ->
                zipStrict loc loc' (checkPExp p c) args argTypes >>= return . Int.TApp returnType name
        | otherwise -> failAt loc "Type Mismatch"
    Nothing -> case find match (constructors p) of
        Just (Ext.PTCon loc' _ _ argTypes) ->
            zipStrict loc loc'  (checkPExp p c) args argTypes >>= return . Int.TCon t name
        Nothing -> failAt loc $
            "Missing Definition: " ++ name ++ " does not construct a " ++ typeName t
  where
    match (Ext.PTCon _loc' returnType n _) = n == name && returnType == t
checkPExp p c (Ext.PDes loc name args inner) t = case find match (destructors p) of
    Nothing -> failAt loc $
        "Missing Definition: no destructor to get " ++ typeName t ++ " from " ++ name
    Just (Ext.PTDes loc' _ _ argTypes innerType) -> do
        tinner <- checkPExp p c inner innerType
        targs <- zipStrict loc loc' (checkPExp p c) args argTypes
        return $ Int.TDes t name targs tinner
  where
    match (Ext.PTDes _loc' r n a _) = n == name && r == t && length a == length args

-- |Infer the type of a term.
inferPExp :: Program -> Context -> Ext.PExp -> Either Error Int.TExp
inferPExp _ context (Ext.PVar loc name) = case lookup name context of
    Nothing  -> failAt loc "Unbound Variable"
    Just typ -> Right (Int.TVar typ name)
inferPExp p c (Ext.PApp loc name args) = case lookup name (functions p) of
    Just (loc', argTypes, returnType) ->
        zipStrict loc loc' (checkPExp p c) args argTypes >>= return . Int.TApp returnType name
    Nothing -> case find match (constructors p) of
        Just (Ext.PTCon loc' returnType _ argTypes) ->
            zipStrict loc loc' (checkPExp p c) args argTypes >>= return . Int.TCon returnType name
        Nothing -> failAt loc "Missing Definition"
  where
    match (Ext.PTCon _loc' _ n _) = n == name
inferPExp p c (Ext.PDes loc name args inner) = do
    tinner <- inferPExp p c inner
    case find (match (texpReturnType tinner)) (destructors p) of
        Nothing -> failAt loc "Missing Definition"
        Just (Ext.PTDes loc' returnType _ argTypes _) -> do
            targs <- zipStrict loc loc' (checkPExp p c) args argTypes
            return $ Int.TDes returnType name targs tinner
  where
    texpReturnType :: Int.TExp -> Int.Type
    texpReturnType (Int.TVar t _) = t
    texpReturnType (Int.TApp t _ _) = t
    texpReturnType (Int.TCon t _ _) = t
    texpReturnType (Int.TDes t _ _ _) = t

    match t' (Ext.PTDes _loc' _ n _ t) = n == name && t == t'

-- |Identify a type to the user.
typeName :: Int.Type -> Identifier
typeName (Int.Type n) = n

-- |Typecheck a rule against the function's signature.
checkPTRule :: Program -> PTSig -> Ext.PTRule -> Either Error Int.Rule
checkPTRule p s (Ext.PTRule loc left right) = do
    tleft <- checkPQ p left s
    let c = tqContext tleft
    tright <- checkPExp p c right (tqReturnType tleft)
    let d = nubContext c
    if length c == length d then
        return (tleft, tright)
    else
        failAt loc "Shadowed Variable"

-- |Fold to collect definitions.
preCheckPT :: Program -> Ext.PT -> Either Error Program
preCheckPT prog@(Program names cons _ _ _) (Ext.PTPos loc name cons')
    | name `elem` names  = failAt loc "Shadowed Definition"
    | any mismatch cons' = failAt loc "Definition Mismatch"
    | otherwise          = Right prog {
          typeNames = (name:names)
        , constructors = cons ++ cons'
        }
  where
    mismatch (Ext.PTCon _loc' returnType _ _) = returnType /= name
preCheckPT prog@(Program names _ des _ _) (Ext.PTNeg loc name des')
    | name `elem` names = failAt loc "Shadowed Definition"
    | any mismatch des' = failAt loc "Definition Mismatch"
    | otherwise         = Right prog {
          typeNames = (name:names)
        , destructors = des ++ des'
        }
  where
    mismatch (Ext.PTDes _loc' _ _ _ innerType) = innerType /= name
preCheckPT prog@(Program _ _ _ funs rulz) (Ext.PTFun loc name argTypes returnType _)
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
postCheckPT :: Program -> Ext.PT -> Either Error Program
postCheckPT prog@(Program names _ _ _ _) (Ext.PTPos loc name cons')
    | any missing cons'  = failAt loc "Missing Definition"
    | otherwise          = Right prog
  where
    missing (Ext.PTCon _loc' _ _ args)        = (nub args) \\ (name:names) /= []
postCheckPT prog@(Program names _ _ _ _) (Ext.PTNeg loc name des')
    | any missing des'  = failAt loc $
        "Missing Definition: " ++ typeName name ++
        " has a destructor with an unknown argument type"
    | otherwise         = Right prog
  where
    missing (Ext.PTDes _loc' _ _ args _)       = (nub args) \\ (name:names) /= []
postCheckPT prog@(Program _ _ _ _ rulz) (Ext.PTFun loc name argTypes returnType rs)
    = do
        let sig = (name, (loc, argTypes, returnType))
        trs <- mapM (checkPTRule prog sig) rs
        return prog {
              rules = ((name, trs):rulz)
            }

-- |Fold to typecheck definitions.
checkPT :: Program -> Ext.PT -> Either Error Program
checkPT prog def = preCheckPT prog def >>= (flip postCheckPT) def

-- |Turn parser output into interpreter input.
typecheck :: [Ext.PT] -> Either Error Int.Rules
typecheck defs = do
    pre  <- foldM preCheckPT emptyProgram defs
    prog <- foldM postCheckPT pre defs
    return $ rules prog
