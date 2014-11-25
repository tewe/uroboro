module Uroboro.Parser
    (
      pmain
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
pmain :: Parser PExp
pmain = whiteSpace *> pexp <* eof

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

-- |Parse data definition
ptpos :: Parser PT
ptpos = liftM PTPos (reserved "data" *> identifier <* reserved "where")
    <*> many1 (liftM PTCon identifier <*> parens (commaSep identifier) <*> (colon *> identifier))

-- |Parse codata definition
ptneg :: Parser PT
ptneg = liftM PTNeg (reserved "codata" *> identifier <* reserved "where")
    <*> many1 (liftM PTDes identifier <*> (dot *> identifier) <*> parens (commaSep identifier) <*> (colon *> identifier))

-- |Parse function definition
ptfun :: Parser PT
ptfun = liftM PTFun (reserved "function" *> identifier) <*> parens (commaSep identifier) <*> (colon *> identifier <* reserved "where")
    <*> many1 (liftM PTRule pq <*> (symbol "=" *> pexp))
