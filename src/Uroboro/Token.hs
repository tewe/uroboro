{-|
Description : Primitive parsers

-}
module Uroboro.Token
    ( -- * Parser type
      Parser
    , parse
    , getLocation
    , exactly
      -- * Parsers for whole tokens
      -- $wholetokens
    , colon
    , commaSep
    , dot
    , identifier
    , parens
    , reserved
    , symbol
      -- * Parser for parts of tokens
      -- $partsoftokens
    , whiteSpace
    ) where

import Control.Applicative ((<$>), (<*>), (<*), (*>))
import Control.Arrow (left)

import Text.Parsec hiding (parse)
import qualified Text.Parsec as Parsec
import Text.Parsec.Error (errorMessages, showErrorMessages)

import Uroboro.Error

-- | Parser without user state.
type Parser = Parsec String ()

-- | Use up all input for one parser.
exactly :: Parser a -> Parser a
exactly parser = whiteSpace *> parser <* eof

-- | Parse something.
parse :: Parser a -> FilePath -> String -> Either Error a
parse parser fname input = left convertError $ Parsec.parse parser fname input

-- Get current location.
getLocation :: Parser Location
getLocation = do
  pos <- getPosition
  return (convertLocation pos)

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

-- $wholetokens
--
-- All parsers in this section parse whole tokens, that is, they
-- automatically skip any 'whiteSpace' after what they accept.

-- | Parser @colon@ accepts the character @\':\'@.
colon      :: Parser String
colon      = symbol ":"

-- | Parser @comma@ accepts the character @\',\'@.
comma      :: Parser String
comma      = symbol ","

-- | Parser @commaSep p@ accepts a comma-separated list of what
-- @p@ accepts.
commaSep   :: Parser a -> Parser [a]
commaSep p = sepBy p comma

-- | Parser @dot@ accepts the character @\'.\'@.
dot        :: Parser String
dot        = symbol "."

-- | Parser @ident@ accepts an identifier or a keyword. Use
-- 'identifier' to parse only identifiers, and 'reserved' to
-- parse only keywords.
ident      :: Parser String
ident      = (:) <$> identStart <*> many identPart

identStart :: Parser Char
identStart = letter <|> oneOf "_"

identPart  :: Parser Char
identPart  = alphaNum <|> oneOf "_'"

-- | Parser @identifier@ accepts identifiers but not keywords.
identifier :: Parser String
identifier = lexeme $ try $ do
  name <- ident
  case name of
    "codata" -> unexpected "keyword 'codata'"
    "data" -> unexpected "keyword 'data'"
    "function" -> unexpected "keyword 'function'"
    "where" -> unexpected "keyword 'where'"
    _ -> return name

-- | Parser @lparen@ accepts the character @\'(\'@.
lparen     :: Parser String
lparen     = symbol "("

-- | Parser @rparen@ accepts the character @\')\'@.
rparen     :: Parser String
rparen     = symbol ")"

-- | Parser @parens p@ accepts what p accepts, in parentheses.
parens     :: Parser a -> Parser a
parens     = between lparen rparen

-- | Parser @reserved s@ accepts keyword @s@.
reserved   :: String -> Parser ()
reserved s =  (lexeme $ try $ string s >> notFollowedBy identPart)
                <?> "keyword '" ++ s ++ "'"

skip       :: Parser a -> Parser ()
skip p     = p *> return ()

symbol     :: String -> Parser String
symbol s   = lexeme (string s)

-- $partsoftokens
--
-- The parsers in this section parse parts of tokens. They don't
-- skip 'whiteSpace' automatically.

-- | Parser @whiteSpace@ skips whitespace and comments.
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
