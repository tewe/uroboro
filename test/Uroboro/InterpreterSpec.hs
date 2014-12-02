module Uroboro.InterpreterSpec
    (
      spec
    ) where

import Test.Hspec

import Uroboro.Interpreter

spec :: Spec
spec = do
    describe "pattern matching" $ do
        it "fills variables" $ do
            let name = "l"
            let term = (TCon "ListOfInt" "empty" [])
            pmatch term (PVar "ListOfInt" name) `shouldBe` Right [(name, term)]
