module Uroboro.Parser
    (
      expression
    , pattern
    ) where

import Control.Monad (liftM)
import Text.Parsec
import qualified Text.Parsec.Token as P

import Uroboro.Language (languageDef)
import Uroboro.Syntax

lexer = P.makeTokenParser languageDef
commaSep = P.commaSep lexer
parens = P.parens lexer
dot = P.dot lexer
reserved = P.reserved lexer
colon = P.colon lexer

identifier = P.identifier lexer

expression = (try application <|> variable) `chainl1` dotOperator

dotOperator = do
    dot
    return op

op :: Exp -> Exp -> Exp
e `op` (Application s es) = DestructorApplication e s es

variable = liftM Variable $ identifier

application = do
    f <- identifier
    es <- parens $ commaSep expression
    return $ Application f es

pattern = try constructorPattern
      <|> variablePattern

variablePattern = liftM VariablePattern $ identifier

constructorPattern = do
    c <- identifier
    ps <- parens $ commaSep pattern
    return $ ConstructorPattern c ps
