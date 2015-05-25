{-|
Description : Typechecker

Typecheck parser output, which turns it into interpreter input.
-}
module Uroboro.Checker
    (
      checkExp
    , preCheckDef
    , postCheckDef
    , checkDef
    , Context
    , emptyProgram
    , inferExp
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
type FunSig = (Identifier, (Location, [Int.Type], Int.Type))

-- |State of the typechecker.
data Program = Program {
      typeNames    :: [Int.Type]  -- Cache types of constructors and destructors.
    , constructors :: [Ext.ConSig]
    , destructors  :: [Ext.DesSig]
    , functions    :: [FunSig] -- Always update functions and rules together.
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
patContext :: Int.Pat -> Context
patContext (Int.VarPat t n) = [(n, t)]
patContext (Int.ConPat _ _ args) = concat $ map patContext args

-- |Extract variable types from a typed copattern.
copContext :: Int.Cop -> Context
copContext (Int.AppCop _ _ args) = concat $ map patContext args
copContext (Int.DesCop _ _ args inner) = concat [copContext inner, concat $ map patContext args]

-- |A zipWithM that requires identical lengths.
zipStrict :: Location -> Location -> (a -> b -> Either Error c) -> [a] -> [b] -> Either Error [c]
zipStrict loc _loc' f a b
  | length a == length b = zipWithM f a b
  | otherwise            = failAt loc "Length Mismatch"

-- |Typecheck a pattern
checkPat :: Program -> Ext.Pat -> Int.Type -> Either Error Int.Pat
checkPat _ (Ext.VarPat _loc name) t = return (Int.VarPat t name)
checkPat p (Ext.ConPat loc name args) t = case find match (constructors p) of
    Just (Ext.ConSig loc' _ _ argTypes) ->
        zipStrict loc loc' (checkPat p) args argTypes >>= return . Int.ConPat t name
    Nothing -> failAt loc "Missing Definition"
  where
    match (Ext.ConSig _loc' returnType n _) = n == name && returnType == t

-- |Typecheck a copattern. Takes hole type.
checkCop :: Program -> Ext.Cop -> FunSig -> Either Error Int.Cop
checkCop p (Ext.AppCop loc name args) (name', (loc', argTypes, returnType))
    | name == name' = do
        targs <- zipStrict loc loc' (checkPat p) args argTypes
        return $ Int.AppCop returnType name targs
    | otherwise     = failAt loc $
        "Definition Mismatch: " ++ name ++ " used in copattern for " ++ name'
checkCop p (Ext.DesCop loc name args inner) s = do
    tinner <- checkCop p inner s
    case find (match (copReturnType tinner)) (destructors p) of
        Nothing -> failAt loc $
            "Missing Definition: " ++ (typeName $ copReturnType tinner) ++ "." ++ name
        Just (Ext.DesSig loc' returnType _ argTypes _) -> do
            targs <- zipStrict loc loc' (checkPat p) args argTypes
            return $ Int.DesCop returnType name targs tinner
  where
    match t (Ext.DesSig _loc' _ n _ innerType) = n == name && innerType == t

-- |The type a copattern matches.
copReturnType :: Int.Cop -> Int.Type
copReturnType (Int.AppCop t _ _) = t
copReturnType (Int.DesCop t _ _ _) = t

-- |Typecheck a term.
checkExp :: Program -> Context -> Ext.Exp -> Int.Type -> Either Error Int.Exp
checkExp _ c (Ext.VarExp loc n) t = case lookup n c of
    Just t' | t' == t   -> return (Int.VarExp t n)
            | otherwise -> failAt loc $ "Type Mismatch: " ++ n ++
                " expected to be " ++ typeName t ++ " but is actually " ++ typeName t'
    Nothing             -> failAt loc $ "Unbound Variable: " ++ n
checkExp p c (Ext.AppExp loc name args) t = case lookup name (functions p) of
    Just (loc', argTypes, returnType) | returnType == t ->
                zipStrict loc loc' (checkExp p c) args argTypes >>= return . Int.AppExp returnType name
        | otherwise -> failAt loc "Type Mismatch"
    Nothing -> case find match (constructors p) of
        Just (Ext.ConSig loc' _ _ argTypes) ->
            zipStrict loc loc'  (checkExp p c) args argTypes >>= return . Int.ConExp t name
        Nothing -> failAt loc $
            "Missing Definition: " ++ name ++ " does not construct a " ++ typeName t
  where
    match (Ext.ConSig _loc' returnType n _) = n == name && returnType == t
checkExp p c (Ext.DesExp loc name args inner) t = case find match (destructors p) of
    Nothing -> failAt loc $
        "Missing Definition: no destructor to get " ++ typeName t ++ " from " ++ name
    Just (Ext.DesSig loc' _ _ argTypes innerType) -> do
        tinner <- checkExp p c inner innerType
        targs <- zipStrict loc loc' (checkExp p c) args argTypes
        return $ Int.DesExp t name targs tinner
  where
    match (Ext.DesSig _loc' r n a _) = n == name && r == t && length a == length args

-- |Infer the type of a term.
inferExp :: Program -> Context -> Ext.Exp -> Either Error Int.Exp
inferExp _ context (Ext.VarExp loc name) = case lookup name context of
    Nothing  -> failAt loc "Unbound Variable"
    Just typ -> Right (Int.VarExp typ name)
inferExp p c (Ext.AppExp loc name args) = case lookup name (functions p) of
    Just (loc', argTypes, returnType) ->
        zipStrict loc loc' (checkExp p c) args argTypes >>= return . Int.AppExp returnType name
    Nothing -> case find match (constructors p) of
        Just (Ext.ConSig loc' returnType _ argTypes) ->
            zipStrict loc loc' (checkExp p c) args argTypes >>= return . Int.ConExp returnType name
        Nothing -> failAt loc "Missing Definition"
  where
    match (Ext.ConSig _loc' _ n _) = n == name
inferExp p c (Ext.DesExp loc name args inner) = do
    tinner <- inferExp p c inner
    case find (match (texpReturnType tinner)) (destructors p) of
        Nothing -> failAt loc "Missing Definition"
        Just (Ext.DesSig loc' returnType _ argTypes _) -> do
            targs <- zipStrict loc loc' (checkExp p c) args argTypes
            return $ Int.DesExp returnType name targs tinner
  where
    texpReturnType :: Int.Exp -> Int.Type
    texpReturnType (Int.VarExp t _) = t
    texpReturnType (Int.AppExp t _ _) = t
    texpReturnType (Int.ConExp t _ _) = t
    texpReturnType (Int.DesExp t _ _ _) = t

    match t' (Ext.DesSig _loc' _ n _ t) = n == name && t == t'

-- |Identify a type to the user.
typeName :: Int.Type -> Identifier
typeName (Int.Type n) = n

-- |Typecheck a rule against the function's signature.
checkRule :: Program -> FunSig -> Ext.Rule -> Either Error Int.Rule
checkRule p s (Ext.Rule loc left right) = do
    tleft <- checkCop p left s
    let c = copContext tleft
    tright <- checkExp p c right (copReturnType tleft)
    let d = nubContext c
    if length c == length d then
        return (tleft, tright)
    else
        failAt loc "Shadowed Variable"

-- |Fold to collect definitions.
preCheckDef :: Program -> Ext.Def -> Either Error Program
preCheckDef prog@(Program names cons _ _ _) (Ext.DatDef loc name cons')
    | name `elem` names  = failAt loc "Shadowed Definition"
    | any mismatch cons' = failAt loc "Definition Mismatch"
    | otherwise          = Right prog {
          typeNames = (name:names)
        , constructors = cons ++ cons'
        }
  where
    mismatch (Ext.ConSig _loc' returnType _ _) = returnType /= name
preCheckDef prog@(Program names _ des _ _) (Ext.CodDef loc name des')
    | name `elem` names = failAt loc "Shadowed Definition"
    | any mismatch des' = failAt loc "Definition Mismatch"
    | otherwise         = Right prog {
          typeNames = (name:names)
        , destructors = des ++ des'
        }
  where
    mismatch (Ext.DesSig _loc' _ _ _ innerType) = innerType /= name
preCheckDef prog@(Program _ _ _ funs rulz) (Ext.FunDef loc name argTypes returnType _)
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
postCheckDef :: Program -> Ext.Def -> Either Error Program
postCheckDef prog@(Program names _ _ _ _) (Ext.DatDef loc name cons')
    | any missing cons'  = failAt loc "Missing Definition"
    | otherwise          = Right prog
  where
    missing (Ext.ConSig _loc' _ _ args)        = (nub args) \\ (name:names) /= []
postCheckDef prog@(Program names _ _ _ _) (Ext.CodDef loc name des')
    | any missing des'  = failAt loc $
        "Missing Definition: " ++ typeName name ++
        " has a destructor with an unknown argument type"
    | otherwise         = Right prog
  where
    missing (Ext.DesSig _loc' _ _ args _)       = (nub args) \\ (name:names) /= []
postCheckDef prog@(Program _ _ _ _ rulz) (Ext.FunDef loc name argTypes returnType rs)
    = do
        let sig = (name, (loc, argTypes, returnType))
        trs <- mapM (checkRule prog sig) rs
        return prog {
              rules = ((name, trs):rulz)
            }

-- |Fold to typecheck definitions.
checkDef :: Program -> Ext.Def -> Either Error Program
checkDef prog def = preCheckDef prog def >>= (flip postCheckDef) def

-- |Turn parser output into interpreter input.
typecheck :: [Ext.Def] -> Either Error Int.Rules
typecheck defs = do
    pre  <- foldM preCheckDef emptyProgram defs
    prog <- foldM postCheckDef pre defs
    return $ rules prog
