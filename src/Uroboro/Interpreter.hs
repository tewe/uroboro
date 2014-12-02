module Uroboro.Interpreter where

import Control.Monad (zipWithM)

import Uroboro.Abstract
import Uroboro.Checker (etype)

pmatch :: TP -> TExp -> Maybe [(String, TExp)]
pmatch (TPVar name type_) exp | type_ == etype exp = return [(name, exp)]
pmatch (TPCons name patterns returnType) (TCon name' exps returnType') | name == name' && length patterns == length exps && returnType == returnType' = do
    substitutions <- zipWithM pmatch patterns exps
    return $ concat substitutions
pmatch _ _ = Nothing

-- |Only normal form will match
qmatch :: TQ -> TExp -> Maybe [(String, TExp)]
qmatch (TQApp name patterns returnType) (TApp name' exps returnType') | name == name' && length patterns == length exps && returnType == returnType' = do
    substitutions <- zipWithM pmatch patterns exps
    return $ concat substitutions
qmatch (TQDes pattern name patterns returnType) (TDes name' exp exps returnType') | name == name' && length patterns == length exps && returnType == returnType' = do
    substitution <- qmatch pattern exp
    substitutions <- zipWithM pmatch patterns exps
    return $ substitution ++ (concat substitutions)
qmatch _ _ = Nothing

eval :: TRules -> [(String, TExp)] -> TExp -> Either String TExp -- TODO no either but throw error
eval _ context (TVar x _) = maybe (Left "sure you type-checked?") return $ lookup x context
--eval rules substitution (TApp name args returnType) =
