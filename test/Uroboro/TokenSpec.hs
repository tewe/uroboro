module Uroboro.TokenSpec
    (
      spec
    ) where

import Test.Hspec

import Uroboro.Error

import Uroboro.Token

import Utils

spec :: Spec
spec = do
    context "when looking for identifiers" $ do
        it "rejects keywords" $ do
            identifier `shouldReject` "codata"
            identifier `shouldReject` "data"
            identifier `shouldReject` "function"
            identifier `shouldReject` "where"
        it "accepts identifiers starting with keywords" $ do
            identifier `shouldAccept` "codatatype"
            identifier `shouldAccept` "datatype"
            identifier `shouldAccept` "functional"
            identifier `shouldAccept` "whereas"
    context "when looking for a keyword" $ do
        it "accepts the keyword" $ do
            reserved "codata" `shouldAccept` "codata"
            reserved "data" `shouldAccept` "data"
            reserved "function" `shouldAccept` "function"
            reserved "where" `shouldAccept` "where"
        it "rejects identifiers starting with the keyword" $ do
            reserved "codata" `shouldReject` "codatatype"
            reserved "data" `shouldReject` "datatype"
            reserved "function" `shouldReject` "functional"
            reserved "where" `shouldReject` "whereas"
    context "when looking for comments" $ do
        it "recognizes end-of-line comments starting with --" $ do
            whiteSpace `shouldAccept` "-- comment \n"
            whiteSpace `shouldReject` "-- comment \nfoo"
        it "recognizes comments between {- and -}" $ do
            whiteSpace `shouldAccept` "{- comment -}"
            whiteSpace `shouldReject` "{- comment -}foo"
        it "recognizes nested comments" $ do
            whiteSpace `shouldAccept` "{- {- -} -}"
            whiteSpace `shouldReject` "{- {- -}"
        it "recognizes {-# LINE #-} pragmas" $ do
            whiteSpace `shouldAccept` "{-# LINE 5 \"foo\" #-}\n"
        it "recognizes invalid pragmas as comments" $ do
            whiteSpace `shouldAccept` "{-# #-}"
            whiteSpace `shouldAccept` "{-# FOO #-}"
            whiteSpace `shouldAccept` "{-# FOO -}"
            whiteSpace `shouldAccept` "{- FOO #-}"
            whiteSpace `shouldAccept` "{-# LINE #-}"
            whiteSpace `shouldAccept` "{-# LINE 5 foo #-}"
            whiteSpace `shouldAccept` "{-# LINE 5 \"foo #-}"
            whiteSpace `shouldAccept` "{-# LINE \"foo\" #-}"
    context "when reporting errors" $ do
        it "includes location information" $ do
            failingAfter "foo" ""
              `shouldReportLocation` "foo:1:1"
            failingAfter "foo" "   "
              `shouldReportLocation` "foo:1:4"
            failingAfter "foo" "\n  "
              `shouldReportLocation` "foo:2:3"
        it "processes pragmas" $ do
            failingAfter "foo" "{-# LINE 1 \"bar\" #-}\n"
              `shouldReportLocation` "bar:1:1"
            failingAfter "foo" "{-# LINE 1 \"bar\" #-}\n   "
              `shouldReportLocation` "bar:1:4"
            failingAfter "foo" "{-# LINE 1 \"bar\" #-}\n\n  "
              `shouldReportLocation` "bar:2:3"
            failingAfter "foo" "\n{-# LINE 5 \"bar\" #-}\n"
              `shouldReportLocation` "bar:5:1"
            failingAfter "foo" "\n{-# LINE 5 \"bar\" #-}\n\n"
              `shouldReportLocation` "bar:6:1"

failingAfter :: FilePath -> String -> Either Error a
failingAfter = parse (exactly (fail ""))

shouldReportLocation :: Either Error a -> String -> Expectation
shouldReportLocation (Left e) loc = show e `shouldContain` loc
shouldReportLocation (Right _) _ =
  expectationFailure "Parsing succeded."
