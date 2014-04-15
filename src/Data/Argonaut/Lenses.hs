{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, BangPatterns #-}

module Data.Argonaut.Lenses
  (
      boolL
    , numberL
    , arrayL
    , objectL
    , stringL
    , nullL
  ) where

import Data.Argonaut.Core
import Control.Lens
import Control.Monad()
import Control.Applicative()
import Data.Scientific (Scientific)
import Data.Typeable(Typeable)
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as M
import qualified Data.Text as T

boolL :: Prism' Json Bool
boolL = prism' fromBool toBool

numberL :: Lens Json Json (Maybe Scientific) Scientific
numberL = lens toScientific (const fromScientific)

arrayL :: Prism' Json JArray
arrayL = prism' fromArray toArray

objectL :: Prism' Json JObject
objectL = prism' fromObject toObject

stringL :: Prism' Json String
stringL = prism' fromString toString

nullL :: Prism' Json ()
nullL = prism' fromUnit toUnit