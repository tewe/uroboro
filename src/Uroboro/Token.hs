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
-- TODO add signatures using our Parser type.

-- |Parser without user state.
type Parser = Parsec String ()

lexer      = P.makeTokenParser languageDef

colon      = P.colon      lexer
commaSep   = P.commaSep   lexer
dot        = P.dot        lexer
identifier = P.identifier lexer
parens     = P.parens     lexer
reserved   = P.reserved   lexer
symbol     = P.symbol     lexer
whiteSpace = P.whiteSpace lexer
