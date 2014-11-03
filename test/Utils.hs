{-# OPTIONS_GHC -fno-warn-orphans #-}

module Utils
    (
      parseFromFile
    ) where

import Text.Parsec (ParseError, parse)

import Uroboro.Exception
import Uroboro.Parser (Parser)

instance Eq ParseError where
   _ == _ = False

-- |Exceptions instead of Either.
parseFromFile :: Parser a -> FilePath -> IO a
parseFromFile parser fname = do
    input <- readFile fname
    case parse parser fname input of
        Left _ -> throwIO ParseException
        Right v -> return v
