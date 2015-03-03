{-|
Description : Primitive parsers

-}
module Uroboro.Token
    (
      Parser
    , colon
    , commaSep
    , dot
    , identifier
    , parens
    , reserved
    , symbol
    , whiteSpace
    ) where

import Control.Applicative ((<$>), (<*>), (<*), (*>))

import Text.Parsec
import Text.Parsec.Char

-- |Parser without user state.
type Parser = Parsec String ()

colon      :: Parser String
colon      = symbol ":"

comma      :: Parser String
comma      = symbol ","

commaSep   :: Parser a -> Parser [a]
commaSep p = sepBy p comma

dot        :: Parser String
dot        = symbol "."

ident      :: Parser String
ident      = (:) <$> identStart <*> many identPart

identStart :: Parser Char
identStart = letter <|> oneOf "_"

identPart  :: Parser Char
identPart  = alphaNum <|> oneOf "_'"

identifier :: Parser String
identifier = lexeme $ try $ do
  name <- ident
  case name of
    "codata" -> unexpected "keyword 'codata'"
    "data" -> unexpected "keyword 'data'"
    "function" -> unexpected "keyword 'function'"
    "where" -> unexpected "keyword 'where'"
    _ -> return name

lparen     :: Parser String
lparen     = symbol "("

rparen     :: Parser String
rparen     = symbol ")"

parens     :: Parser a -> Parser a
parens     = between lparen rparen

reserved   :: String -> Parser ()
reserved s =  (lexeme $ try $ string s >> notFollowedBy identPart)
                <?> "keyword '" ++ s ++ "'"

skip       :: Parser a -> Parser ()
skip p     = p *> return ()

symbol     :: String -> Parser String
symbol s   = lexeme (string s)

whiteSpace :: Parser ()
whiteSpace = skipMany (skip space <|> single <|> multi <?> "") where
  single = do
    try (string "--") *> skipMany (noneOf "\r\n")
  multi = try (string "{-") *> nested
  nested =
    char '-' *> (char '}' *> return () <|>
                 nested) <|>
    char '{' *> (char '-' *> nested *> nested <|>
                 nested) <|>
    anyChar  *> nested

lexeme :: Parser a -> Parser a
lexeme p = p <* whiteSpace
