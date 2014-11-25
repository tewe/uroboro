module Uroboro.ParserSpec
    (
      spec
    ) where

import Data.Either (isRight)
import Text.Parsec (parse)

import Test.Hspec

import Uroboro.Parser
import Uroboro.Tree

import Paths_uroboro
import Utils()

spec :: Spec
spec = do
    describe "parser" $ do
        it "recognizes the prelude" $ do
            fname <- getDataFileName "samples/prelude.uro"
            input <- readFile fname
            parse parseDef fname input `shouldSatisfy` isRight
    describe "command line" $ do
        it "ignores whitespace" $ do
            parse parseExp "" "  x  " `shouldBe` Right (PVar "x")
        it "uses the longest match" $ do
            parse parseExp "" "x()" `shouldBe` Right (PApp "x" [])
