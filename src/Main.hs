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
          | Typecheck [PT]
          | Evaluate  [PT] PExp

getOpt :: Mode -> String -> IO Mode
getOpt _ _ = return Help

main :: IO ()
main = do
    args <- getArgs
    mode <- foldM getOpt Help args
    case mode of
        Help -> do
            putStrLn "USAGE: uroboro files... [expression]"
            exitFailure
        Typecheck _ -> putStrLn "check"
        Evaluate _ _ -> putStrLn "eval"
