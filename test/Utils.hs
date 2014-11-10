{-# OPTIONS_GHC -fno-warn-orphans #-}

module Utils
    (
      parseFromFile
    , parseString
    ) where

import Text.Parsec (ParseError, parse)

import Uroboro.Exception
import Uroboro.Parser (Parser)

instance Eq ParseError where
   _ == _ = False

-- |Exceptions instead of Either.
parseIO :: Parser a -> String -> String -> IO a
parseIO parser fname input = case parse parser fname input of
    Left _ -> throwIO ParseException
    Right v -> return v

parseFromFile :: Parser a -> FilePath -> IO a
parseFromFile parser fname = do
    input <- readFile fname
    parseIO parser fname input

parseString :: Parser a -> String -> IO a
parseString parser input = parseIO parser "" input
