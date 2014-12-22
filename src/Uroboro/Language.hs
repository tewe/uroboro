{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

{-|
Description : Basic parser configuration

This sets up the Parsec token parser. We use a separate module to avoid filling
the namespace with record accessors.
-}
module Uroboro.Language
    (
      languageDef
    ) where

import Text.Parsec.Language (haskellStyle)
import Text.Parsec.Token (reservedNames)

-- |Comments and identifiers work just like in Haskell.
languageDef = haskellStyle
    {
      reservedNames  = ["data", "codata", "function", "where"]
    }
