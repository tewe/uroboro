{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Uroboro.Language
    (
      languageDef
    ) where

import Text.Parsec.Token
import Text.Parsec.Language (haskellStyle)

languageDef = haskellStyle
    {
      reservedNames  = ["data", "codata", "function", "where"]
    }
