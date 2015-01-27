{-# OPTIONS_GHC -fno-warn-orphans #-}

module Uroboro.ParserSpec
    (
      spec
    ) where

import Data.Either (isRight)
import Text.Parsec (parse)

import Test.Hspec

import Paths_uroboro (getDataFileName)
import Uroboro.Parser (parseDef, parseExp, pq)
import Uroboro.Tree
    (
      PExp(..)
    , PQ(..)
    , PT(..)
    , PTCon(..)
    , PTDes(..)
    , Type(..)
    )

spec :: Spec
spec = do
    describe "pq" $ do
        it "gets selector order right" $ do
            parse pq "" "fib().tail().head() = succ(zero())" `shouldSatisfy` (\x ->
              case x of
                Right (PQDes "head" [] (PQDes "tail" [] (PQApp "fib" []))) -> True
                _ -> False)
    describe "parser" $ do
        it "recognizes the prelude" $ do
            fname <- getDataFileName "samples/prelude.uro"
            input <- readFile fname
            parse parseDef fname input `shouldSatisfy` isRight
        it "observes argument order (constructor)" $ do
            let source = "data Int where zero(): Int"
            parse parseDef "" source `shouldSatisfy` (\x -> case x of
              Right [PTPos (Type "Int") [PTCon (Type "Int") "zero" []]] -> True
              _ -> False)
        it "observes argument order (destructor)" $ do
            let source = "codata StreamOfInt where StreamOfInt.head(): Int"
            parse parseDef "" source `shouldSatisfy` (\x -> case x of
              Right [PTNeg (Type "StreamOfInt") [PTDes (Type "Int") "head" [] (Type "StreamOfInt")]] -> True
              _ -> False)
    describe "command line" $ do
        it "ignores whitespace" $ do
            parse parseExp "" "  x  " `shouldSatisfy` (\x -> case x of
              Right (PVar "x") -> True
              _ -> False)
        it "uses the longest match" $ do
            parse parseExp "" "x()" `shouldSatisfy` (\x -> case x of
              Right (PApp "x" []) -> True
              _ -> False)
