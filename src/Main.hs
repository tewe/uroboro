import System.Console.GetOpt
import System.Environment

import Text.Parsec (parse)

import Paths_uroboro
import Uroboro.Checker (typecheck, inferPExp)
import Uroboro.Interpreter (eval)
import Uroboro.Parser (parseDef, parseExp)
import Uroboro.Tree

data Flag = SkipPrelude
          | MainExpr String
          | LoadLib FilePath deriving (Eq, Show)

options :: [OptDescr Flag]
options =
    [ Option ['p'] [] (NoArg SkipPrelude) "do not include prelude"
    , Option ['e'] [] (ReqArg MainExpr "EXPRESSION") "evaluate EXPRESSION"
    ]

mainOpts :: [String] -> IO [Flag]
mainOpts argv = case getOpt Permute options argv of
    (flags, paths, []) -> return $ flags ++ (map LoadLib paths)
    (_, _, errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where
    header = "Usage: uroboro [OPTION...] files..."

main :: IO ()
main = getArgs >>= mainOpts >>= putStrLn . show
