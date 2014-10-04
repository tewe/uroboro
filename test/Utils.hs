{-# OPTIONS_GHC -fno-warn-orphans #-}

module Utils
    (
    ) where

import Text.Parsec (ParseError)

instance Eq ParseError where
   _ == _ = False
