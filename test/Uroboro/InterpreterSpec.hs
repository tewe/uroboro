module Uroboro.InterpreterSpec
    (
      spec
    ) where

import Test.Hspec

import Uroboro.Abstract
import Uroboro.Interpreter

spec :: Spec
spec = do
    describe "patterns" $ do
        it "variables match anything" $ do
            let v = TCon "empty" [] "ListOfInt"
            pmatch (TPVar "l" "ListOfInt") v `shouldBe` Just [("l", v)]
        it "constructors match" $ do
            let x = TCon "zero" [] "Int"
            let xs = TCon "empty" [] "ListOfInt"
            let v = TCon "cons" [x, xs] "ListOfInt"
            pmatch (TPCons "cons" [TPVar "x" "Int", TPVar "xs" "ListOfInt"] "ListOfInt") v `shouldBe` Just [("x", x), ("xs", xs)]
    describe "copatterns" $ do
        it "function application to data matches" $ do
            let add1 = TVar "add1" "IntToInt" -- TODO feels wrong
            let empty = TCon "empty" [] "ListOfInt"
            qmatch (TQApp "map" [TPVar "f" "IntToInt", TPCons "empty" [] "ListOfInt"] "ListOfInt") (TApp "map" [add1, empty] "ListOfInt") `shouldBe` Just [("f", add1)]
        it "match" $ do
            let hole = TQApp "add1" [] "IntToInt"
            let pattern = TQDes hole "apply" [TPVar "x" "Int"] "Int"
            let zero = TCon "zero" [] "Int"
            let exp = TDes "apply" (TApp "add1" [] "IntToInt") [zero] "Int"
            qmatch pattern exp `shouldBe` Just [("x", zero)]
