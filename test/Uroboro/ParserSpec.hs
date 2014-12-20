{-# OPTIONS_GHC -fno-warn-orphans #-}

module Uroboro.ParserSpec
    (
      spec
    ) where

import Data.Either (isRight)
import Text.Parsec (parse, ParseError)

import Test.Hspec

import Uroboro.Parser
import Uroboro.Tree

import Paths_uroboro

instance Eq ParseError where
    _ == _ = False

spec :: Spec
spec = do
    describe "pq" $ do
        it "gets selector order right" $ do
            parse pq "" "fib().tail().head() = succ(zero())" `shouldBe` Right
                (PQDes "head" [] (PQDes "tail" [] (PQApp "fib" [])))
    describe "parser" $ do
        let int = Type "Int"
        it "recognizes the prelude" $ do
            fname <- getDataFileName "samples/prelude.uro"
            input <- readFile fname
            parse parseDef fname input `shouldSatisfy` isRight
        it "observes argument order (constructor)" $ do
            let source = "data Int where zero(): Int"
            let parsed = PTPos int [PTCon int "zero" []]
            parse parseDef "" source `shouldBe` Right [parsed]
        it "observes argument order (destructor)" $ do
            let stream = Type "StreamOfInt"
            let source = "codata StreamOfInt where StreamOfInt.head(): Int"
            let parsed = PTNeg stream [PTDes int "head" [] stream]
            parse parseDef "" source `shouldBe` Right [parsed]
    describe "command line" $ do
        it "ignores whitespace" $ do
            parse parseExp "" "  x  " `shouldBe` Right (PVar "x")
        it "uses the longest match" $ do
            parse parseExp "" "x()" `shouldBe` Right (PApp "x" [])
