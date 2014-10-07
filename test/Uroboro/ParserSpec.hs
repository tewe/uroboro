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

    describe "pattern" $ do
        it "parses constructor patterns" $ do
            parse pattern "" "cons(x, xs)" `shouldBe` Right
                (ConstructorPattern "cons"
                    [VariablePattern "x", VariablePattern "xs"])
        it "nests" $ do
            parse pattern "" "cons(x, cons(y, empty))" `shouldBe` Right
                (ConstructorPattern "cons" [VariablePattern "x",
                    (ConstructorPattern "cons"
                        [VariablePattern "y", VariablePattern "empty"])])

    describe "dataDefinition" $ do
        it "parses data types" $ do
            parse dataDefinition "" "data ListOfInt where empty(): ListOfInt"
                `shouldBe` Right (DataDefinition "ListOfInt"
                    [Signature "empty" [] "ListOfInt"])
        it "handles line breaks" $ do
            parse dataDefinition "" "data ListOfInt where\nempty(): ListOfInt"
                `shouldBe` Right (DataDefinition "ListOfInt"
                    [Signature "empty" [] "ListOfInt"])
        it "handles tabs" $ do
            parse dataDefinition "" "data ListOfInt where\n\tempty(): ListOfInt"
                `shouldBe` Right (DataDefinition "ListOfInt"
                    [Signature "empty" [] "ListOfInt"])
        it "parses multiple constructors" $ do
            parse dataDefinition "" "data ListOfInt where \
                \   empty(): ListOfInt \
                \   cons(Int, ListOfInt): ListOfInt"
                `shouldBe` Right (DataDefinition "ListOfInt"
                    [Signature "empty" [] "ListOfInt",
                    Signature "cons" ["Int", "ListOfInt"] "ListOfInt"])
