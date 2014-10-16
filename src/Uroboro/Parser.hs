module Uroboro.Parser
    (
      expression
    , pattern
    , dataDefinition
    , codataDefinition
    , functionDefinition
    , library
    ) where

import Control.Monad (liftM)
import Text.Parsec
import qualified Text.Parsec.Token as P

import Uroboro.Language (languageDef)
import Uroboro.Syntax

type Parser = Parsec String ()

lexer = P.makeTokenParser languageDef
commaSep = P.commaSep lexer
parens = P.parens lexer
dot = P.dot lexer
reserved = P.reserved lexer
colon = P.colon lexer
symbol = P.symbol lexer
lexeme = P.lexeme lexer

identifier = P.identifier lexer
type_ = identifier

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

constructor = do
    c <- identifier
    ts <- parens $ commaSep type_
    colon
    t <- type_
    return $ Signature c ts t

dataDefinition = do
    reserved "data"
    d <- type_
    reserved "where"
    cs <- many1 constructor
    return $ DataDefinition d cs

selector :: String -> Parser Signature
selector c = do
    lexeme $ string c
    dot
    s <- identifier
    ts <- parens $ commaSep type_
    colon
    t <- type_
    return $ Signature s ts t

codataDefinition = do
    reserved "codata"
    c <- type_
    reserved "where"
    s <- many1 $ selector c
    return $ CodataDefinition c s

destructorCopattern :: Copattern -> Parser Copattern
destructorCopattern q = do {
    ; dot
    ; s <- identifier
    ; ps <- parens $ commaSep pattern
    ; let c = DestructorCopattern q s ps
    ; option c (destructorCopattern c)
    } <|> return q

rule :: String -> Parser Rule
rule f = do
    lexeme $ string f
    ps <- parens $ commaSep pattern
    c <- destructorCopattern (Hole ps)
    symbol "="
    e <- expression
    return $ Rule c e

functionDefinition = do
    reserved "function"
    f <- identifier
    ts <- parens $ commaSep type_
    colon
    t <- type_
    reserved "where"
    ps <- many1 $ rule f
    return $ FunctionDefinition (Signature f ts t) ps

definition = choice
    [ dataDefinition
    , codataDefinition
    , functionDefinition
    ]

library :: Parser Library
library = many definition
