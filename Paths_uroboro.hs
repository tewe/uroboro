{-|
Description : Mock out Cabal path abstraction

This is a mock copy of something Cabal generates during installation.
-}
module Paths_uroboro
    (
      getDataFileName
    ) where

-- |Adjust path for Cabal moving things around.
getDataFileName :: FilePath -> IO FilePath
getDataFileName = return
