module Uroboro.CheckerSpec
    (
      spec
    ) where

import Control.Monad (foldM)
import Data.Either (isRight)

import Test.Hspec

import Uroboro.Parser (parseDef)
import Uroboro.Checker
import Utils (parseString)

-- |Assert error message
shouldFail :: Show a => Either String a -> String -> Expectation
Left msg `shouldFail` prefix = takeWhile (/= ':') msg `shouldBe` prefix
Right  x `shouldFail` prefix = expectationFailure
    ("expected: " ++ prefix ++ "\n but got: " ++ show x)

spec :: Spec
spec = do
    describe "pos" $ do
        it "checks return types" $ do
            x:_ <- parseString parseDef "data Int where zero(): Float"
            checkPT emptyProgram x `shouldFail` "Definition Mismatch"
        it "prevents duplicates" $ do
            defs <- parseString parseDef $ unlines
                [ "data Int where zero(): Int"
                , "data Int where succ(): Int"
                ]
            foldM checkPT emptyProgram defs `shouldFail` "Shadowed Definition"
        it "allows data types" $ do
            x:_ <- parseString parseDef "data Int where zero(): Int"
            checkPT emptyProgram x `shouldSatisfy` isRight
    describe "neg" $ do
        let stream = "codata StreamOfInt where StreamOfInt.head(): Int"
        it "prevents duplicates" $ do
            defs <- parseString parseDef $ unlines [stream, stream]
            foldM checkPT emptyProgram defs `shouldFail` "Shadowed Definition"
        it "checks argument types" $ do
            x:_ <- parseString parseDef "codata IntToInt where IntToInt.apply(Int): Int"
            checkPT emptyProgram x `shouldFail` "Missing Definition"
        it "allows codata types" $ do
            x:_ <- parseString parseDef stream
            checkPT emptyProgram x `shouldSatisfy` isRight
