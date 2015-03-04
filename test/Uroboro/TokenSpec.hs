module Uroboro.TokenSpec
    (
      spec
    ) where

import Test.Hspec

import Uroboro.Token

import Utils

spec :: Spec
spec = do
    context "when looking for identifiers" $ do
        it "supports identifiers that start with keywords" $ do
            identifier `shouldAccept` "whereas"
    context "when looking for comments" $ do
        it "recognizes end-of-line comments starting with --" $ do
            whiteSpace `shouldAccept` "-- comment \n"
        it "recognizes comments between {- and -}" $ do
            whiteSpace `shouldAccept` "{- comment -}"
        it "recognizes nested comments" $ do
            whiteSpace `shouldAccept` "{- {- -} -}"
