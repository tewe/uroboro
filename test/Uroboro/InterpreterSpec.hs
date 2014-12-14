module Uroboro.InterpreterSpec
    (
      spec
    ) where

import Test.Hspec
import Text.Parsec (parse)

import Paths_uroboro
import Uroboro.Checker (typecheck, inferPExp)
import Uroboro.CheckerSpec (prelude, shouldFail)
import Uroboro.Interpreter
import Uroboro.Parser (parseDef, parseExp)
import Uroboro.Tree

import Utils (parseString)

rules :: IO Rules
rules = do
    fname <- getDataFileName "samples/prelude.uro"
    input <- readFile fname
    case parse parseDef fname input of
        Left _ -> fail "Parser"
        Right defs -> case typecheck defs of
            Left _ -> fail "Checker"
            Right p -> return p

main :: String -> IO TExp
main input = do
    pexp <- parseString parseExp input
    prog <- prelude
    case inferPExp prog [] pexp of
        Left _ -> fail "Checker"
        Right texp -> return texp

spec :: Spec
spec = do
    describe "pattern matching" $ do
        it "fills variables" $ do
            let name = "l"
            let t = Type "ListOfInt"
            let term = (TCon t "empty" [])
            pmatch term (TPVar t name) `shouldBe` Right [(name, term)]
    describe "reduction" $ do
        it "stops" $ do
            p <- rules
            reduce p (TCon (Type "Int") "zero" []) `shouldFail` "Not a redex"
        it "works" $ do
            p <- rules
            m <- main "add(zero(), succ(zero()))"
            let int = Type "Int"
            reduce p m `shouldBe` Right (TApp int "add"
                [TCon int "succ" [TCon int "zero" []], TCon int "zero" []])
