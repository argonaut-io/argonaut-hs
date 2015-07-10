module Data.Argonaut.Printer
  ( Printer
  , printJson
  ) where

import Prelude
import Data.Argonaut.Core
import Data.Function

class Printer a where
  printJson :: Json -> a

instance printerString :: Printer String where
  printJson = show
