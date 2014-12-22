{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Uroboro.Language
    (
      languageDef
    ) where

import Text.Parsec.Language (haskellStyle)
import Text.Parsec.Token (reservedNames)

languageDef = haskellStyle
    {
      reservedNames  = ["data", "codata", "function", "where"]
    }
