module Uroboro.CheckerSpec
    (
      spec
    ) where

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
    it "can look up constructors" $ do
        findConstructor library "c" `shouldBe` Just (["T"], "D")
    it "can look up destructors" $ do
        findDestructor library "C" "head" `shouldBe` Just (["T1"], "T2")
    it "can look up functions" $ do
        findFunction library "f" `shouldBe` Just (["T"], "T")
    describe "variables" $ do
        it "may be unknown" $ do
            typecheck [] [] (Variable "x") "Int" `shouldBe` Left "unknown"
        it "may not match" $ do
            typecheck [] [("x", "Char")] (Variable "x") "Int" `shouldBe` Left "mismatch"
        it "may check out" $ do
            let e = Variable "x"
            typecheck [] [("x", "Int")] e "Int" `shouldBe` Right e
