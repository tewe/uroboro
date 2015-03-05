module Uroboro.ParserSpec
    (
      spec
    ) where

import Data.Either (isRight)
import Text.Parsec (parse)

import Test.Hspec

import Paths_uroboro (getDataFileName)
import Uroboro.Parser (parseDef, parseExp, pq)
import Uroboro.Tree.External

spec :: Spec
spec = do
    describe "pq" $ do
        it "gets selector order right" $ do
            parse pq "" "fib().tail().head() = succ(zero())" `shouldSatisfy` (\x ->
              case x of
                Right (PQDes _ "head" [] (PQDes _ "tail" [] (PQApp _ "fib" []))) -> True
                _ -> False)
    describe "parser" $ do
        it "recognizes the prelude" $ do
            fname <- getDataFileName "samples/prelude.uro"
            input <- readFile fname
            parse parseDef fname input `shouldSatisfy` isRight
        it "observes argument order (constructor)" $ do
            let source = "data Int where zero(): Int"
            parse parseDef "" source `shouldSatisfy` (\x -> case x of
              Right [PTPos _ (Type "Int") [PTCon _ (Type "Int") "zero" []]] -> True
              _ -> False)
        it "observes argument order (destructor)" $ do
            let source = "codata StreamOfInt where StreamOfInt.head(): Int"
            parse parseDef "" source `shouldSatisfy` (\x -> case x of
              Right [PTNeg _ (Type "StreamOfInt") [PTDes _ (Type "Int") "head" [] (Type "StreamOfInt")]] -> True
              _ -> False)
        it "accepts empty functions" $ do
            let source = "function foo() : Foo where"
            parse parseDef "" source `shouldSatisfy` (\x -> case x of
              Right [PTFun _ "foo" [] (Type "Foo") []] -> True
              _ -> False)
        it "accepts empty data types" $ do
            let source = "data Foo where"
            parse parseDef "" source `shouldSatisfy` (\x -> case x of
              Right [PTPos _ (Type "Foo") []] -> True
              _ -> False)
        it "accepts empty codata types" $ do
            let source = "codata Foo where"
            parse parseDef "" source `shouldSatisfy` (\x -> case x of
              Right [PTNeg _ (Type "Foo") []] -> True
              _ -> False)
    describe "command line" $ do
        it "ignores whitespace" $ do
            parse parseExp "" "  x  " `shouldSatisfy` (\x -> case x of
              Right (PVar _ "x") -> True
              _ -> False)
        it "uses the longest match" $ do
            parse parseExp "" "x()" `shouldSatisfy` (\x -> case x of
              Right (PApp _ "x" []) -> True
              _ -> False)
