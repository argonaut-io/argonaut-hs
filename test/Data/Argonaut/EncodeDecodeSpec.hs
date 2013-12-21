{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Data.Argonaut.EncodeDecodeSpec where

import Data.Argonaut
import Data.Argonaut.TestTemplates
import Data.Argonaut.TestInstances()
import Test.Hspec

spec :: Spec
spec = parallel $ do
  $(buildEncodeDecodeTest [
    ''JString
    , ''Bool
    , ''Double
    , ''JArray
    , ''JObject])
