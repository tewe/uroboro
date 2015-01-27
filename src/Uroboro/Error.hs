{-|
Description : Store and format error messages
-}
module Uroboro.Error
    ( Error (MakeError)
    ) where

import Data.List (intercalate)

data Error = MakeError FilePath Int Int String

instance Show Error where
  show (MakeError name line column message) =
    intercalate ":"
      [ name
      , show line
      , show column
      , " Syntax Error"
      ] ++ (unlines $ map ("  " ++) $ lines $ message)
