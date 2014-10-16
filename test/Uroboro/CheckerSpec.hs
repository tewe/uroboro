module Uroboro.CheckerSpec
    (
      spec
    ) where

import Test.Hspec

import Uroboro.Syntax
import Uroboro.Checker (typecheck)

spec :: Spec
spec = do
    describe "variables" $ do
        it "may be unknown" $ do
            typecheck [] [] (Variable "x") "Int" `shouldBe` Left "unknown"
        it "may not match" $ do
            typecheck [] [("x", "Char")] (Variable "x") "Int" `shouldBe` Left "mismatch"
        it "may check out" $ do
            typecheck [] [("x", "Int")] e "Int" `shouldBe` Right e
          where e = Variable "x"
