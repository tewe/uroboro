module Uroboro.CheckerSpec
    (
      spec
    ) where

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
            pos [] x `shouldFail` "Definition Mismatch"
        it "prevents duplicates" $ do
            x:xs <- parseString parseDef $ unlines
                [ "data Int where zero(): Int"
                , "data Int where succ(): Int"
                ]
            pos xs x `shouldFail` "Shadowed Definition"
        it "folds" $ do
            x:_ <- parseString parseDef "data Int where zero(): Int"
            pos [] x `shouldBe` (Right [x])
