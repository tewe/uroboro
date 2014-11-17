module Uroboro.CheckerSpec
    (
      spec
    ) where

import Data.Either (isLeft, isRight)

import Test.Hspec

import Paths_uroboro
import Uroboro.Parser
import Uroboro.Syntax
import Uroboro.Abstract
import Uroboro.Checker
import Utils

prelude = getDataFileName "samples/prelude.uro" >>= parseFromFile library

-- |Context using prelude
c = [
      ("i", "Int")
    , ("f", "IntToInt")
    , ("g", "TwoIntToInt")
    , ("l", "ListOfInt")
    , ("s", "StreamOfInt")
    ]

spec :: Spec
spec = do
    describe "terms" $ do
        context "when variables" $ do
            it "may be unknown" $ do
                check [] c (Variable "x") "Int" `shouldBe` Left "unknown"
            it "may not match" $ do
                check [] c (Variable "f") "Int" `shouldBe` Left "mismatch"
            it "may check out" $ do
                let e = Variable "i"
                check [] c e "Int" `shouldBe` Right (TVar "i" "Int")
        context "when constructors" $ do
            it "may check out" $ do
                p <- prelude
                let e = ConstructorApplication "succ" [Variable "i"]
                check p c e "Int" `shouldSatisfy` isRight
            it "check the return type" $ do
                p <- prelude
                let e = ConstructorApplication "succ" [Variable "i"]
                check p c e "T" `shouldBe` Left "unknown"
            it "check the number of arguments" $ do
                p <- prelude
                let e = ConstructorApplication "succ" [Variable "i", Variable "s"]
                check p c e "Int" `shouldBe` Left "wrong number of arguments"
            it "recurse" $ do
                p <- prelude
                let e = ConstructorApplication "succ" [Variable "s"]
                check p c e "Int" `shouldBe` Left "mismatch"
            it "may not exist" $ do
                p <- prelude
                let e = ConstructorApplication "full" []
                check p c e "ListOfInt" `shouldBe` Left "unknown"
        context "when destructors" $ do
            it "may check out" $ do
                p <- prelude
                e <- parseString expression "f.apply(i)"
                check p c e "Int" `shouldBe` Right (TDes "apply" (TVar "f" "IntToInt") [TVar "i" "Int"] "Int")
        context "when functions" $ do
            it "may check out" $ do
                p <- prelude
                check p [("f", "IntToInt"), ("l", "ListOfInt")] (FunctionApplication "map" [Variable "f", Variable "l"]) "ListOfInt" `shouldBe` Right (TApp "map" [TVar "f" "IntToInt", TVar "l" "ListOfInt"] "ListOfInt")
        context "when applications" $ do
            it "infer as functions" $ do
                p <- prelude
                e <- parseString expression "map(f, l)"
                check p c e "ListOfInt" `shouldBe` Right (TApp "map" [TVar "f" "IntToInt", TVar "l" "ListOfInt"] "ListOfInt")
            it "infer as constructors" $ do
                p <- prelude
                e <- parseString expression "succ(i)"
                check p c e "Int" `shouldBe` Right (TCon "succ" [TVar "i" "Int"] "Int")
    describe "patterns" $ do
        it "bind variables" $ do
            checkp [] (VariablePattern "x") "T" `shouldBe` Right (TPVar "x" "T")
        it "nest" $ do
            p <- prelude
            e <- parseString pattern "succ(i)"
            checkp p e "Int" `shouldBe` Right (TPCons "succ" [TPVar "i" "Int"] "Int")
    describe "copatterns" $ do
        it "can be holes" $ do
            checkc [] (Signature "chr" ["Int"] "Char") (Hole [VariablePattern "i"]) `shouldBe` Right (TQApp "chr" [TPVar "i" "Int"] "Char")
    describe "typecheck" $ do
        it "checks functions" $ do
            p <- prelude
            typecheck p (last p) `shouldSatisfy` isRight -- TODO don't depend on order
