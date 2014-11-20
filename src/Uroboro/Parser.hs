module Uroboro.Parser
    (
      pexp
    ) where

import Control.Applicative ((<*), (*>))
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

-- |block start where lines
def :: String -> Parser a -> Parser b -> Parser (a, [b])
def block start line = do
    _ <- reserved block
    h <- start
    _ <- reserved "where"
    l <- many1 line
    return (h, l)

pvar :: Parser PExp
pvar = liftM PVar (identifier)

-- |app(arg, ...)
papp :: Parser PExp
papp = liftM (uncurry PApp) (call pexp)

-- |exp().des(arg, ...)
pdes :: Parser PExp
pdes = do
    e <- try papp <|> pvar <?> "function or variable"
    _ <- dot
    sepBy1 (call pexp) dot  >>= return . (foldl makePdes e)
  where makePdes exp (name, args) = PDes name args exp

-- |Parse expressions/terms
pexp :: Parser PExp
pexp = try pdes
   <|> try papp
   <|> pvar
   <?> "expression"

-- |Parse command line
pmain :: Parser PExp
pmain = whiteSpace *> pexp <* eof
