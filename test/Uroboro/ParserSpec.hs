module Uroboro.ParserSpec
    (
      spec
    ) where

import Data.Either (isLeft)
import Text.Parsec (parse)

import Test.Hspec

import Uroboro.Parser
import Uroboro.Syntax
import Utils()

spec :: Spec
spec = do
    describe "expression" $ do
        it "does not parse operators" $ do
            parse expression "" "+" `shouldSatisfy` isLeft
        it "parses variables" $ do
            parse expression "" "x" `shouldBe` Right (Variable "x")
        it "recognizes applications" $ do
            parse expression "" "f()" `shouldBe` Right (Application "f" [])
            parse expression "" "e.s()" `shouldBe` Right
                (DestructorApplication (Variable "e") "s" [])
        it "nests" $ do
            parse expression "" "f(x)" `shouldBe` Right
                (Application "f" [Variable "x"])
        it "ignores whitespace" $ do
            parse expression "" "f ( x ) " `shouldBe` Right
                (Application "f" [Variable "x"])
            parse expression "" " f(x)" `shouldSatisfy` isLeft
