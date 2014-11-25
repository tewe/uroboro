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

-- |No user state
type Parser = ParsecT String () Identity

-- |identifier(parser, ...)
call :: Parser a -> Parser (String, [a])
call parser = do
    i <- identifier
    p <- parens (commaSep parser)
    return (i, p)

-- |z().des(x, ...)
des :: Parser a -> Parser b -> (String -> [b] -> a -> a) -> Parser a
des pz px constructor = do
    z <- pz
    _ <- dot
    sepBy1 (call px) dot >>= return . (foldl make z)
  where make e (name, args) = constructor name args e

pvar :: Parser PExp
pvar = liftM PVar (identifier)

-- |app(arg, ...)
papp :: Parser PExp
papp = liftM (uncurry PApp) (call pexp)

-- |exp().des(arg, ...)
pdes :: Parser PExp
pdes = des (try papp <|> pvar <?> "function or variable") pexp PDes

-- |Parse expressions/terms
pexp :: Parser PExp
pexp = try pdes
   <|> try papp
   <|> pvar
   <?> "expression"

-- |Parse command line
pmain :: Parser PExp
pmain = whiteSpace *> pexp <* eof

-- |Parse pattern
pp :: Parser PP
pp = try ppcon
 <|> ppvar
 <?> "pattern"
 where
   ppvar = liftM PPVar (identifier)
   ppcon = liftM (uncurry PPCon) (call pp)

-- |Parse hole that starts a copattern
pqapp :: Parser PQ
pqapp = liftM (uncurry PQApp) (call pp)

-- |hole().des(arg, ...)
pqdes :: Parser PQ
pqdes = des (pqapp <?> "function") pp PQDes

-- |Parse copattern
pq :: Parser PQ
pq = try pqdes
 <|> pqapp
 <?> "copattern"

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
