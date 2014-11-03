{-# LANGUAGE DeriveDataTypeable #-}

module Uroboro.Exception
    (
      UroboroException(..)
    , throwIO
    ) where

import Control.Exception
import Data.Typeable (Typeable)

data UroboroException = ParseException deriving (Show, Typeable)

instance Exception UroboroException
