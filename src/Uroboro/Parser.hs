{-|
Description : Parse string into parse tree

Parsec applicative style.
-}
module Uroboro.Parser
    (
      -- * Parsing Uroboro
      parseFile
    , parseExpression
      -- * Individual parsers
    , parseDef
    , parseExp
    , Parser
    , pq
    ) where

import Control.Applicative ((<*), (<*>), (*>))
import Control.Arrow (left)
import Control.Monad (liftM)

import Data.List (intercalate)

import Text.Parsec
import Text.Parsec.Error (errorMessages, showErrorMessages)

import Uroboro.Token
import Uroboro.Tree
    (
      PExp(..)
    , PP(..)
    , PQ(..)
    , PT(..)
    , PTCon(..)
    , PTDes(..)
    , PTRule(..)
    , Type(..)
    )

-- | Parse whole file.
parseFile :: FilePath -> String -> Either Error [PT]
parseFile fname input = left convertError $ parse parseDef fname input

-- | Parse expression.
parseExpression :: FilePath -> String -> Either Error PExp
parseExpression fname input = left convertError $ parse parseExp fname input

-- |Parser without user state.
type Parser = Parsec String ()

-- |Parse "(p, ...)".
args :: Parser a -> Parser [a]
args p = parens (commaSep p)

-- |Recursively apply a list of functions to a start value, from left to right.
fold :: a -> [a -> a] -> a
fold x [] = x
fold x (f:fs) = f (fold x fs)

-- |Parse "a.name(b, ...)...".
dotNotation :: (String -> [b] -> a -> a) -> Parser a -> Parser b -> Parser a
dotNotation make a b = liftM fold_ a <*> (dot *> sepBy1 name dot)
            where name = liftM make identifier <*> args b
                  fold_ x l = fold x (reverse l)            -- TODO make fold into foldr.

-- |Parse expression.
pexp :: Parser PExp
pexp = choice [des, app, var] <?> "expression"
  where
    des = try $ dotNotation PDes (app <|> var <?> "function or variable") pexp
    app = try $ liftM PApp identifier <*> args pexp
    var = liftM PVar identifier

-- | Use up all input for one parser.
exactly :: Parser a -> Parser a
exactly parser = whiteSpace *> parser <* eof

-- |Parse exactly one expression.
parseExp :: Parser PExp
parseExp = exactly pexp

-- |Parse pattern.
pp :: Parser PP
pp = choice [con, var] <?> "pattern"
  where
    con = try $ liftM PPCon identifier <*> args pp
    var = liftM PPVar identifier

-- |Parse copattern.
pq :: Parser PQ
pq = choice [des, app] <?> "copattern"
  where
    des = try $ dotNotation PQDes (app <?> "function") pp
    app = liftM PQApp identifier <*> args pp

-- |Parse whole file.
parseDef :: Parser [PT]
parseDef = exactly $ many (choice [pos, neg, fun])
  where
    pos = definition "data" PTPos <*> where1 con
    neg = definition "codata" PTNeg <*> where1 des
    fun = liftM PTFun (reserved "function" *> identifier) <*>
        args typ <*> (colon *> typ) <*> where1 rul

    con = liftM (flip3 PTCon) identifier <*> args typ <*> (colon *> typ)
    des = liftM (flip4 PTDes) typ <*>
        (dot *> identifier) <*> args typ <*> (colon *> typ)
    rul = liftM PTRule pq <*> (symbol "=" *> pexp)

    typ :: Parser Type
    typ = liftM Type identifier

    flip3 f a b c   = f c a b
    flip4 f a b c d = f d b c a

    definition :: String -> (Type -> a) -> Parser a
    definition kind make = liftM make (reserved kind *> typ)

    where1 :: Parser a -> Parser [a]
    where1 a = reserved "where" *> many1 a

-- | Custom error type (to modify the Show instance)
data Error = MakeError FilePath Int Int String

instance Show Error where
  show (MakeError name line column message) =
    intercalate ":"
      [ name
      , show line
      , show column
      , " Syntax Error"
      ] ++ (unlines $ map ("  " ++) $ lines $ message)

-- | Convert error to custom error type
convertError :: ParseError -> Error
convertError err = MakeError name line column messages where
  pos = errorPos err
  name = sourceName pos
  line = sourceLine pos
  column = sourceColumn pos
  messages = showErrorMessages
               "or" "unknown parse error" "expecting"
               "unexpected" "end of input"
               (errorMessages err)
