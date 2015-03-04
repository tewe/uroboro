{-|
Description : Parse string into parse tree

Parsec applicative style.
-}
module Uroboro.Parser
    (
      -- * Parsing Uroboro
      parseFile
    , parseExpression
    , parse
      -- * Individual parsers
    , parseDef
    , parseExp
    , Parser
    , pq
    ) where

import Control.Applicative ((<*>), (*>))
import Control.Arrow (left)
import Control.Monad (liftM)

import Text.Parsec
import Text.Parsec.Error (errorMessages, showErrorMessages)

import Uroboro.Error
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

-- | Parse something.
parse :: Parser a -> FilePath -> String -> Either Error a
parse parser fname input = left convertError $ parse parser fname input

-- | Parse whole file.
parseFile :: FilePath -> String -> Either Error [PT]
parseFile = parse parseDef

-- | Parse expression.
parseExpression :: FilePath -> String -> Either Error PExp
parseExpression = parse parseExp

-- |Parse "(p, ...)".
args :: Parser a -> Parser [a]
args p = parens (commaSep p)

-- |Recursively apply a list of functions to a start value, from left to right.
fold :: a -> [a -> a] -> a
fold x [] = x
fold x (f:fs) = f (fold x fs)

-- |Variant of liftM that also stores the current location
liftLoc :: (Location -> a -> b) -> Parser a -> Parser b
liftLoc make parser = do
  pos <- getPosition
  let loc = convertLocation pos
  arg <- parser
  return (make loc arg)

-- |Parse "a.name(b, ...)...".
dotNotation :: (Location -> String -> [b] -> a -> a) -> Parser a -> Parser b -> Parser a
dotNotation make a b = liftM fold_ a <*> (dot *> sepBy1 name dot)
            where name = liftLoc make identifier <*> args b
                  fold_ x l = fold x (reverse l)            -- TODO make fold into foldr.

-- |Parse expression.
pexp :: Parser PExp
pexp = choice [des, app, var] <?> "expression"
  where
    des = try $ dotNotation PDes (app <|> var <?> "function or variable") pexp
    app = try $ liftLoc PApp identifier <*> args pexp
    var = liftLoc PVar identifier

-- |Parse exactly one expression.
parseExp :: Parser PExp
parseExp = exactly pexp

-- |Parse pattern.
pp :: Parser PP
pp = choice [con, var] <?> "pattern"
  where
    con = try $ liftLoc PPCon identifier <*> args pp
    var = liftLoc PPVar identifier

-- |Parse copattern.
pq :: Parser PQ
pq = choice [des, app] <?> "copattern"
  where
    des = try $ dotNotation PQDes (app <?> "function") pp
    app = liftLoc PQApp identifier <*> args pp

-- |Parse whole file.
parseDef :: Parser [PT]
parseDef = exactly $ many (choice [pos, neg, fun])
  where
    pos = definition "data" PTPos <*> where1 con
    neg = definition "codata" PTNeg <*> where1 des
    fun = liftLoc PTFun (reserved "function" *> identifier) <*>
        args typ <*> (colon *> typ) <*> where1 rul

    con = liftLoc (flip3 PTCon) identifier <*> args typ <*> (colon *> typ)
    des = liftLoc (flip4 PTDes) typ <*>
        (dot *> identifier) <*> args typ <*> (colon *> typ)
    rul = liftLoc PTRule pq <*> (symbol "=" *> pexp)

    typ :: Parser Type
    typ = liftM Type identifier

    flip3 f loc a b c   = f loc c a b
    flip4 f loc a b c d = f loc d b c a

    definition :: String -> (Location -> Type -> a) -> Parser a
    definition kind make = liftLoc make (reserved kind *> typ)

    where1 :: Parser a -> Parser [a]
    where1 a = reserved "where" *> many1 a

-- | Convert location to custom location type
convertLocation :: SourcePos -> Location
convertLocation pos = MakeLocation name line column where
  name = sourceName pos
  line = sourceLine pos
  column = sourceColumn pos

-- | Convert error to custom error type
convertError :: ParseError -> Error
convertError err = MakeError location messages where
  pos = errorPos err
  location = convertLocation pos
  messages = showErrorMessages
               "or" "unknown parse error" "expecting"
               "unexpected" "end of input"
               (errorMessages err)
