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
import Control.Monad (liftM)

import Text.Parsec hiding (parse)
import Text.Parsec.Error (errorMessages, showErrorMessages)

import Uroboro.Error
import Uroboro.Token
import Uroboro.Tree.External
    (
      Exp(..)
    , Pat(..)
    , Cop(..)
    , Def(..)
    , ConSig(..)
    , DesSig(..)
    , Rule(..)
    , Type(..)
    )

-- | Parse whole file.
parseFile :: FilePath -> String -> Either Error [Def]
parseFile = parse parseDef

-- | Parse expression.
parseExpression :: FilePath -> String -> Either Error Exp
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
  loc <- getLocation
  arg <- parser
  return (make loc arg)

-- |Parse "a.name(b, ...)...".
dotNotation :: (Location -> String -> [b] -> a -> a) -> Parser a -> Parser b -> Parser a
dotNotation make a b = liftM fold_ a <*> (dot *> sepBy1 name dot)
            where name = liftLoc make identifier <*> args b
                  fold_ x l = fold x (reverse l)            -- TODO make fold into foldr.

-- |Parse expression.
pexp :: Parser Exp
pexp = choice [des, app, var] <?> "expression"
  where
    des = try $ dotNotation DesExp (app <|> var <?> "function or variable") pexp
    app = try $ liftLoc AppExp identifier <*> args pexp
    var = liftLoc VarExp identifier

-- |Parse exactly one expression.
parseExp :: Parser Exp
parseExp = exactly pexp

-- |Parse pattern.
pp :: Parser Pat
pp = choice [con, var] <?> "pattern"
  where
    con = try $ liftLoc ConPat identifier <*> args pp
    var = liftLoc VarPat identifier

-- |Parse copattern.
pq :: Parser Cop
pq = choice [des, app] <?> "copattern"
  where
    des = try $ dotNotation DesCop (app <?> "function") pp
    app = liftLoc AppCop identifier <*> args pp

-- |Parse whole file.
parseDef :: Parser [Def]
parseDef = exactly $ many (choice [pos, neg, fun])
  where
    pos = definition "data" DatDef <*> where1 con
    neg = definition "codata" CodDef <*> where1 des
    fun = liftLoc FunDef (reserved "function" *> identifier) <*>
        args typ <*> (colon *> typ) <*> where1 rul

    con = liftLoc (flip3 ConSig) identifier <*> args typ <*> (colon *> typ)
    des = liftLoc (flip4 DesSig) typ <*>
        (dot *> identifier) <*> args typ <*> (colon *> typ)
    rul = liftLoc Rule pq <*> (symbol "=" *> pexp)

    typ :: Parser Type
    typ = liftM Type identifier

    flip3 f loc a b c   = f loc c a b
    flip4 f loc a b c d = f loc d b c a

    definition :: String -> (Location -> Type -> a) -> Parser a
    definition kind make = liftLoc make (reserved kind *> typ)

    where1 :: Parser a -> Parser [a]
    where1 a = reserved "where" *> many a

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
