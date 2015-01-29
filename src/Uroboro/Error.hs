{-|
Description : Store and format error messages
-}
module Uroboro.Error
    ( Error (MakeError)
    , Location (MakeLocation)
    , failAt
    ) where

import Data.List (intercalate)

data Error = MakeError Location String

data Location = MakeLocation FilePath Int Int

instance Show Location where
  show (MakeLocation name line column) =
    intercalate ":"
      [ name
      , show line
      , show column
      ]

instance Show Error where
  show (MakeError location message) =
    intercalate ":"
      [ show location
      , " Syntax Error"
      ] ++ (unlines $ map ("  " ++) $ lines $ message)

-- |Fail the monad, but with location.
failAt :: Location -> String -> Either Error a
failAt location message = Left $ MakeError location message
