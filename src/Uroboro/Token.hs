{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

{-|
Description : Primitive parsers

We operate above the character level, so we don't have to deal with variable whitespace.
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

import Text.Parsec (Parsec)
import qualified Text.Parsec.Token as P -- TODO have Haddock use those docstrings.

import Uroboro.Language (languageDef)

-- |Parser without user state.
type Parser = Parsec String ()

lexer      = P.makeTokenParser languageDef

colon      :: Parser String
colon      = P.colon      lexer

commaSep   :: Parser a -> Parser [a]
commaSep   = P.commaSep   lexer

dot        :: Parser String
dot        = P.dot        lexer

identifier :: Parser String
identifier = P.identifier lexer

parens     :: Parser a -> Parser a
parens     = P.parens     lexer

reserved   :: String -> Parser ()
reserved   = P.reserved   lexer

symbol     :: String -> Parser String
symbol     = P.symbol     lexer

whiteSpace :: Parser ()
whiteSpace = P.whiteSpace lexer
