{-|
Description : Typecheck and run Uroboro code on the command line

This is the executable comprising the user interface to the interpreter.
-}
module Main
    (
      getOpt
    , main
    , Mode(..)
    ) where

import Control.Monad (foldM, foldM_, zipWithM)
import System.Environment (getArgs)
import System.Exit (exitFailure)

import Text.Parsec (parse)

import Uroboro.Checker (checkPT, emptyProgram, inferPExp, rules)
import Uroboro.Interpreter (eval)
import Uroboro.Parser (parseDef, parseExp, Parser)
import Uroboro.PrettyPrint (render)
import Uroboro.Tree (PT)

-- |How the program operates, and on what data.
data Mode = Help
          | Typecheck [FilePath]
          | Evaluate  [FilePath] String deriving (Eq, Show)

-- |Parse command line options.
getOpt :: [String] -> Mode
getOpt args = case break (== "--") args of
    ([], _)     -> Help
    (x, [])     -> Typecheck x
    (x, [_, y]) -> Evaluate x y
    _           -> Help

-- |For a left value, print it and set the exit code.
eitherIO :: Show a => Either a b -> IO b
eitherIO (Left e)  = do
    print e             -- TODO strip surrounding quotes, or use custom errors
    exitFailure
eitherIO (Right b) = return b

-- |Turn Either monad from parser into IO monad by communicating errors to the user.
parseIO :: Parser a -> String -> String -> IO a
parseIO parser fname input = eitherIO $ parse parser fname input

-- |Load libraries.
parseFiles :: [FilePath] -> IO [PT]
parseFiles paths = do
    lol <- mapM readFile paths >>= zipWithM (parseIO parseDef) paths
    return $ concat lol

-- |Parse given source code, typecheck it, and optionally run it.
-- No output means typechecking was successful.
main :: IO ()
main = do
    args <- getArgs
    case getOpt args of
        Evaluate paths input -> do
            defs  <- parseFiles paths
            pexp  <- eitherIO $ parse parseExp "command line" input

            prog <- eitherIO $ foldM checkPT emptyProgram defs
            texp  <- eitherIO $ inferPExp prog [] pexp

            putStrLn (render $ eval (rules prog) texp)
        Typecheck paths -> do
            defs  <- parseFiles paths
            eitherIO $ foldM_ checkPT emptyProgram defs
        Help -> do
            putStrLn "USAGE: uroboro FILES [-- EXPRESSION]"
            exitFailure
