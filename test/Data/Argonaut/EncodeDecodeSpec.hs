{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Data.Argonaut.EncodeDecodeSpec where

import Data.Argonaut
import Data.Argonaut.Printer()
import Data.Argonaut.TestTemplates
import Data.Argonaut.TestInstances()
import Data.Scientific (Scientific)
import Test.Hspec

spec :: Spec
spec = parallel $ do
  $(buildEncodeDecodeTest [
      ''JString
    , ''JObject
    , ''Bool
    , ''Scientific
    , ''JArray
    , ''()])
