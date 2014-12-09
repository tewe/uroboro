module Uroboro.Parser
    (
      parseDef
    , parseExp
    , Parser
    ) where

import Control.Applicative ((<*), (<*>), (*>))
import Control.Monad (liftM)
import Control.Monad.Identity (Identity)
import Text.Parsec

import Uroboro.Token
import Uroboro.Tree

-- |Parser without user state
type Parser = ParsecT String () Identity

-- |Parse "(p, ...)"
args :: Parser a -> Parser [a]
args p = parens (commaSep p)

-- |Recursively apply a list of functions to a start value, from left to right
fold :: a -> [a -> a] -> a
fold x [] = x
fold x (f:fs) = f (fold x fs)

-- |Parse "a.name(b, ...)..."
dotNotation :: (String -> [b] -> a -> a) -> Parser a -> Parser b -> Parser a
dotNotation make a b = liftM fold a <*> (dot *> sepBy1 name dot)
            where name = liftM make identifier <*> args b

-- |Parse expression
pexp :: Parser PExp
pexp = choice [des, app, var] <?> "expression"
  where
    des = try $ dotNotation PDes (app <|> var <?> "function or variable") pexp
    app = try $ liftM PApp identifier <*> args pexp
    var = liftM PVar identifier

-- |Parse exactly one expression
parseExp :: Parser PExp
parseExp = whiteSpace *> pexp <* eof

-- |Parse pattern
pp :: Parser PP
pp = choice [con, var] <?> "pattern"
  where
    con = try $ liftM PPCon identifier <*> args pp
    var = liftM PPVar identifier

-- |Parse copattern
pq :: Parser PQ
pq = choice [des, app] <?> "copattern"
  where
    des = try $ dotNotation PQDes (app <?> "function") pp
    app = liftM PQApp identifier <*> args pp

-- |Parse whole file
parseDef :: Parser [PT]
parseDef = whiteSpace *> many (choice [pos, neg, fun]) <* eof
  where
    pos = definition "data" PTPos <*> where1 con
    neg = definition "codata" PTNeg <*> where1 des
    fun = definition "function" PTFun <*>
        args typ <*> (colon *> typ) <*> where1 rul

    con = liftM (flip3 PTCon) identifier <*> args typ <*> (colon *> typ)
    des = liftM (flip4 PTDes) typ <*>
        (dot *> identifier) <*> args typ <*> (colon *> typ)
    rul = liftM PTRule pq <*> (symbol "=" *> pexp)

    -- |For readability.
    typ :: Parser Type
    typ = identifier

    flip3 f a b c   = f c a b
    flip4 f a b c d = f d b c a

    definition :: String -> (String -> a) -> Parser a
    definition kind make = liftM make (reserved kind *> typ)

    where1 :: Parser a -> Parser [a]
    where1 a = reserved "where" *> many1 a
