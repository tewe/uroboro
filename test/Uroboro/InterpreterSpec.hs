module Uroboro.InterpreterSpec
    (
      spec
    ) where

import Test.Hspec
import Text.Parsec (parse)

import Paths_uroboro
import Uroboro.Checker (typecheck)
import Uroboro.CheckerSpec (shouldFail)
import Uroboro.Interpreter
import Uroboro.Parser (parseDef)
import Uroboro.Tree

prelude :: IO Rules
prelude = do
    fname <- getDataFileName "samples/prelude.uro"
    input <- readFile fname
    case parse parseDef fname input of
        Left _ -> fail "Parser"
        Right defs -> case typecheck defs of
            Left _ -> fail "Checker"
            Right p -> return p

spec :: Spec
spec = do
    describe "pattern matching" $ do
        it "fills variables" $ do
            let name = "l"
            let t = Type "ListOfInt"
            let term = (TCon t "empty" [])
            pmatch term (TPVar t name) `shouldBe` Right [(name, term)]
    describe "reduction" $ do
        it "" $ do
            p <- prelude
            reduce p (TCon (Type "Int") "zero" []) `shouldFail` "Not a redex"
