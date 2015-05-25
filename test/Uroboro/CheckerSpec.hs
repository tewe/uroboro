module Uroboro.CheckerSpec
    (
      spec
    , prelude
    , shouldFail
    ) where

import Control.Monad (foldM)
import Data.Either (isLeft, isRight)

import Test.Hspec
import Text.Parsec (parse)

import Paths_uroboro (getDataFileName)
import Uroboro.Checker
    (
      checkExp
    , checkDef
    , Context
    , emptyProgram
    , inferExp
    , Program
    )
import Uroboro.Error
import Uroboro.Parser (parseDef, parseExp)
import Uroboro.Tree.Internal (Exp(..), Type(..))
import Utils (parseString)

prelude :: IO Program
prelude = do
    fname <- getDataFileName "samples/prelude.uro"
    input <- readFile fname
    case parse parseDef fname input of
        Left _ -> fail "Parser"
        Right defs -> case foldM checkDef emptyProgram defs of
            Left _ -> fail "Checker"
            Right p -> return p

-- |Context using prelude
c :: Context
c = [
      ("i", Type "Int")
    , ("f", Type "IntToInt")
    , ("g", Type "TwoIntToInt")
    , ("l", Type "ListOfInt")
    , ("s", Type "StreamOfInt")
    ]

-- |Assert error message
shouldFail :: Show a => Either Error a -> String -> Expectation
Left (MakeError _ msg) `shouldFail` prefix = takeWhile (/= ':') msg `shouldBe` prefix
Right  x `shouldFail` prefix = expectationFailure
    ("expected: " ++ prefix ++ "\n but got: " ++ show x)

spec :: Spec
spec = do
    describe "too few arguments" $ do
        it "constructors" $ do
            p <- prelude
            e <- parseString parseExp "succ()"
            checkExp p [] e (Type "Int") `shouldFail` "Length Mismatch"
        it "calls (data)" $ do
            p <- prelude
            e <- parseString parseExp "map()"
            checkExp p [] e (Type "ListOfInt") `shouldFail` "Length Mismatch"
        it "calls (codata)" $ do
            p <- prelude
            e <- parseString parseExp "mapStream().head()"
            checkExp p [] e (Type "StreamOfInt") `shouldSatisfy` isLeft
    describe "checkPT (data)" $ do
        it "checks return types" $ do
            x:_ <- parseString parseDef "data Int where zero(): Float"
            checkDef emptyProgram x `shouldFail` "Definition Mismatch"
        it "prevents duplicates" $ do
            defs <- parseString parseDef $ unlines
                [ "data Int where zero(): Int"
                , "data Int where succ(): Int"
                ]
            foldM checkDef emptyProgram defs `shouldFail` "Shadowed Definition"
        it "allows data types" $ do
            x:_ <- parseString parseDef "data Int where zero(): Int"
            checkDef emptyProgram x `shouldSatisfy` isRight
        it "allows multiple arguments with the same type" $ do
            x:_ <- parseString parseDef "data A where a(A, A): A"
            checkDef emptyProgram x `shouldSatisfy` isRight
    describe "checkDef (codata)" $ do
        let stream = "codata StreamOfInt where StreamOfInt.head(): Int"
        it "prevents duplicates" $ do
            defs <- parseString parseDef $ unlines [stream, stream]
            foldM checkDef emptyProgram defs `shouldFail` "Shadowed Definition"
        it "checks argument types" $ do
            x:_ <- parseString parseDef "codata IntToInt where IntToInt.apply(Int): Int"
            checkDef emptyProgram x `shouldFail` "Missing Definition"
        it "allows codata types" $ do
            x:_ <- parseString parseDef stream
            checkDef emptyProgram x `shouldSatisfy` isRight
        it "allows multiple arguments with the same type" $ do
            x:_ <- parseString parseDef "codata A where A.a(A, A): A"
            checkDef emptyProgram x `shouldSatisfy` isRight
    describe "checkExp" $ do
        it "infers construction" $ do
            p <- prelude
            e <- parseString parseExp "empty()"
            checkExp p [] e (Type "ListOfInt") `shouldSatisfy` (\x -> case x of
              Right (ConExp (Type "ListOfInt") "empty" []) -> True
              _ -> False)
        it "infers applications" $ do
            p <- prelude
            e <- parseString parseExp "map(f, l)"
            checkExp p c e (Type "ListOfInt") `shouldSatisfy` (\x -> case x of
              Right (AppExp (Type "ListOfInt") "map"
                [VarExp (Type "IntToInt") "f", VarExp (Type "ListOfInt") "l"]) -> True
              _ -> False)
    describe "inferPExp" $ do
        it "infers construction" $ do
            p <- prelude
            e <- parseString parseExp "empty()"
            inferExp p [] e `shouldSatisfy` (\x -> case x of
              Right (ConExp (Type "ListOfInt") "empty" []) -> True
              _ -> False)
        it "infers applications" $ do
            p <- prelude
            e <- parseString parseExp "map(f, l)"
            inferExp p c e `shouldSatisfy` (\x -> case x of
              Right (AppExp (Type "ListOfInt") "map"
                [VarExp (Type "IntToInt") "f", VarExp (Type "ListOfInt") "l"]) -> True
              _ -> False)
