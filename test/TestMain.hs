module TestMain
    (
      spec
    ) where

import Test.Hspec

import Main (getOpt, Mode(..))

spec :: Spec
spec = do
    describe "getOpt" $ do
        it "accepts one path" $ do
            let p = ["foo"]
            getOpt p `shouldBe` Typecheck p
        it "accepts two paths" $ do
            let p = ["foo", "bar"]
            getOpt p `shouldBe` Typecheck p
        it "accepts expression" $ do
            getOpt ["foo", "--", "bar"] `shouldBe` Evaluate ["foo"] "bar"
        it "rejects expressions" $ do
            getOpt ["foo", "--", "bar", "x"] `shouldBe` Help
        it "requires paths" $ do
            getOpt ["--", "bar"] `shouldBe` Help
        it "is unforgiving" $ do
            getOpt ["foo", "--"] `shouldBe` Help
