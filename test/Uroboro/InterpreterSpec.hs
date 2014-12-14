module Uroboro.InterpreterSpec
    (
      spec
    ) where

import Test.Hspec

import Uroboro.Interpreter
import Uroboro.Tree

spec :: Spec
spec = do
    describe "pattern matching" $ do
        it "fills variables" $ do
            let name = "l"
            let t = Type "ListOfInt"
            let term = (TCon t "empty" [])
            pmatch term (TPVar t name) `shouldBe` Right [(name, term)]
