{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Uroboro.Token
    (
      colon
    , commaSep
    , dot
    , identifier
    , parens
    , reserved
    , symbol
    , whiteSpace
    ) where

import qualified Text.Parsec.Token as P

import Uroboro.Language (languageDef)

lexer      = P.makeTokenParser languageDef

colon      = P.colon      lexer
commaSep   = P.commaSep   lexer
dot        = P.dot        lexer
identifier = P.identifier lexer
parens     = P.parens     lexer
reserved   = P.reserved   lexer
symbol     = P.symbol     lexer
whiteSpace = P.whiteSpace lexer
