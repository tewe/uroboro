import System.Environment

import Text.Parsec (parse)

import Uroboro.Parser (parseDef)

parseFromFile p fname = do
    input <- readFile fname
    return $ parse p fname input

main :: IO ()
main = do
    args <- getArgs
    lib <- parseFromFile parseDef (args !! 0)
    case lib of
        Left e -> print e
        Right _ -> putStrLn $ "We can't type check yet, but your" ++
            " code was recognized :)"
