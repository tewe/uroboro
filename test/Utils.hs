module Utils
    (
      parseFromFile
    , parseString
    ) where


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
