module Uroboro.TokenSpec
    (
      spec
    ) where

import Text.Parsec (parse)

import Test.Hspec

import Uroboro.Parser (parseExp)
import Uroboro.Tree
    (
      PExp(..)
    )

spec :: Spec
spec = do
    describe "when looking for identifiers" $ do
        it "supports identifiers that start with keywords" $ do
            parse parseExp "" "whereas" `shouldSatisfy` (\x -> case x of
              Right (PVar _ "whereas") -> True
              _ -> False)
    describe "when looking for comments" $ do
        it "recognizes end-of-line comments starting with --" $ do
            parse parseExp "" "-- comment \nx" `shouldSatisfy` (\x -> case x of
              Right (PVar _ "x") -> True
              _ -> False)
        it "recognizes comments between {- and -}" $ do
            parse parseExp "" "{- comment -}x" `shouldSatisfy` (\x -> case x of
              Right (PVar _ "x") -> True
              _ -> False)
        it "recognizes nested comments" $ do
            parse parseExp "" "{- {- -} -}x" `shouldSatisfy` (\x -> case x of
              Right (PVar _ "x") -> True
              _ -> False)
