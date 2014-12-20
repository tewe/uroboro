import Control.Exception (try)
import Control.Monad (foldM)
import System.Environment (getArgs)
import System.Exit (exitFailure)

import Text.Parsec (parse)

import Uroboro.Checker (typecheck, inferPExp)
import Uroboro.Interpreter (eval)
import Uroboro.Parser (parseDef, parseExp)
import Uroboro.Tree

data Mode = Help
          | Typecheck [FilePath]
          | Evaluate  [FilePath] String deriving (Eq, Show)

getOpt :: [String] -> Mode
getOpt args = case break (== "--") args of
    ([], _)     -> Help
    (x, [])     -> Typecheck x
    (x, [_, y]) -> Evaluate x y
    _           -> Help

main :: IO ()
main = do
    args <- getArgs
    case getOpt args of
        Evaluate _ _ -> putStrLn "eval"
        Typecheck _ -> putStrLn "typecheck"
        Help -> do
            putStrLn "USAGE: uroboro FILES [-- EXPRESSION]"
            exitFailure
