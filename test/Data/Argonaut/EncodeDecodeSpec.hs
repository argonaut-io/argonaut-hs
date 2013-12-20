{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Data.Argonaut.EncodeDecodeSpec where

import Data.Argonaut
import Data.Argonaut.TestTemplates
import Data.Argonaut.TestInstances()
import Test.Hspec

spec :: Spec
spec = do
  $(buildEncodeDecodeTest [''String, ''Bool, ''Double, ''JArray, ''JObject])
