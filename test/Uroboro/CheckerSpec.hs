module Uroboro.CheckerSpec
    (
      spec
    ) where

import Data.Either (isLeft, isRight)

import Test.Hspec

import Uroboro.Syntax
import Uroboro.Checker

library :: Library
library = [
      DataDefinition "D" [Signature "c" ["T"] "D"]
    , FunctionDefinition (Signature "f" ["T"] "T")
         [Rule (Hole [VariablePattern "x"]) (Variable "x")]
    , CodataDefinition "C" [Signature "head" ["T1"] "T2"]
    ]

spec :: Spec
spec = do
    describe "variables" $ do
        it "may be unknown" $ do
            check [] [] (Variable "x") "Int" `shouldBe` Left "unknown"
        it "may not match" $ do
            check [] [("x", "Char")] (Variable "x") "Int" `shouldBe` Left "mismatch"
        it "may check out" $ do
            let e = Variable "x"
            check [] [("x", "Int")] e "Int" `shouldBe` Right (TVar "x" "Int")
    describe "constructors" $ do
        it "may check out" $ do
            check library [("x", "T")] (ConstructorApplication "c" [Variable "x"]) "D" `shouldSatisfy` isRight
        it "check the return type" $ do
            check library [("x", "T")] (ConstructorApplication "c" [Variable "x"]) "H" `shouldBe` Left "unknown"
        it "check the number of arguments" $ do
            check library [("x", "T")] (ConstructorApplication "c" [Variable "x", Variable "y"]) "D" `shouldBe` Left "wrong number of arguments"
        it "recurse" $ do
            check library [("x", "A")] (ConstructorApplication "c" [Variable "x"]) "D" `shouldBe` Left "mismatch"
        it "may not exist" $ do
            check library [("x", "T")] (ConstructorApplication "d" [Variable "x"]) "D" `shouldBe` Left "unknown"
    describe "destructors" $ do
        it "may check out" $ do
            check library [("x", "C"), ("y", "T1")] (DestructorApplication (Variable "x") "head" [Variable "y"]) "T2" `shouldBe` Right (TDes "head" (TVar "x" "C") [TVar "y" "T1"] "T2")
    describe "functions" $ do
        it "may check out" $ do
            check library [("x", "T")] (FunctionApplication "f" [Variable "x"]) "T" `shouldBe` Right (TApp "f" [TVar "x" "T"] "T")
    describe "applications inference" $ do
        it "finds functions" $ do
            check library [("x", "T")] (Application "f" [Variable "x"]) "T" `shouldBe` Right (TApp "f" [TVar "x" "T"] "T")
        it "finds constructors" $ do
            check library [("x", "T")] (ConstructorApplication "c" [Variable "x"]) "D" `shouldBe` Right (TCon "c" [TVar "x" "T"] "D")
