import System.Environment

import Text.Parsec (parse)

import Uroboro.Parser (expression)

main :: IO ()
main = do
    args <- getArgs
    case parse expression "(argument)" (args !! 0) of
        Left e -> print e
        Right x -> print x
