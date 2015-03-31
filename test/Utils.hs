module Utils
    (
      parseFromFile
    , parseString
    , shouldAccept
    , shouldReject
    ) where

import Control.Applicative ((<*))

import Test.Hspec

import Text.Parsec (eof)

import Uroboro.Parser (parse, Parser)

-- |Exceptions instead of Either.
parseIO :: Parser a -> String -> String -> IO a
parseIO parser fname input = case parse parser fname input of
    Left _ -> ioError $ userError ""
    Right v -> return v

parseFromFile :: Parser a -> FilePath -> IO a
parseFromFile parser fname = do
    input <- readFile fname
    parseIO parser fname input

parseString :: Parser a -> String -> IO a
parseString parser input = parseIO parser "" input

shouldAccept :: Parser a -> String -> Expectation
shouldAccept p s = case parse (p <* eof) "<testcase>" s of
  Left e -> expectationFailure (show e)
  Right _ -> return ()

shouldReject :: Show a => Parser a -> String -> Expectation
shouldReject p s = case parse (p <* eof) "<testcase>" s of
  Left _ -> return ()
  Right x -> expectationFailure $
               "Parsing \"" ++ s ++ "\" succeded with result " ++ show x ++ "."
