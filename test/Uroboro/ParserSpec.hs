module Uroboro.ParserSpec
    (
      spec
    ) where

import Data.Either (isLeft, isRight)
import Text.Parsec (parse)

import Test.Hspec

import Uroboro.Parser
import Uroboro.Syntax
import Utils()

mapCode :: String
mapCode = unlines [
    "function map(IntToInt, ListOfInt): ListOfInt where",
    "    map(f, empty()) = empty()",
    "    map(f, cons(x, xs)) = cons(f.apply(x), map(f, xs))"
	]

mapTree :: Definition
mapTree = FunctionDefinition
    (Signature "map" ["IntToInt", "ListOfInt"] "ListOfInt")
    [
    Rule
        [VariablePattern "f", ConstructorPattern "empty" []]
        []
        (Application "empty" []),
    Rule
        [
            VariablePattern "f",
            ConstructorPattern "cons" [VariablePattern "x", VariablePattern "xs"]
        ]
        []
        (Application "cons" [
            DestructorApplication (Variable "f") "apply" [Variable "x"],
            Application "map" [Variable "f", Variable "xs"]
        ])
    ]

mapStreamCode :: String
mapStreamCode = unlines [
    "function mapStream(IntToInt, StreamOfInt): StreamOfInt where",
    "    mapStream(f, s).head() = f.apply(s.head())",
    "    mapStream(f, s).tail() = mapStream(f, s.tail())"
    ]

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

    describe "codataDefinition" $ do
        it "parses codata types" $ do
            parse codataDefinition "" "codata StreamOfInt where StreamOfInt.head(): Int"
                `shouldBe` Right (CodataDefinition "StreamOfInt" [Signature "head" [] "Int"])
        it "rejects hole name mismatches" $ do
            parse codataDefinition "" "codata StreamOfInt where Bogus.head(): Int"
                `shouldSatisfy` isLeft

    describe "functionDefinition" $ do
        it "parses functions" $ do
            parse functionDefinition "" mapCode `shouldBe` Right mapTree
        it "recognizes copatterns" $ do
            parse functionDefinition "" mapStreamCode `shouldSatisfy` isRight
    it "recognizes programs" $ do
        parse library "" (mapCode ++ "\n" ++ mapStreamCode) `shouldSatisfy` isRight
