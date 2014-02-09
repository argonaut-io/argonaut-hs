{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Data.Argonaut.ParserSpec where

import Data.Argonaut.Parser
import Data.Argonaut.TestInstances()
import qualified Data.Text as T
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  describe "parseString" $ do
    it "for some valid json, produces the same value" $ do
      property $ \originalJson ->
        let
          asString = T.pack $ show originalJson
          parsedJson = parseString asString
        in parsedJson `shouldBe` (StringErrorParseSuccess originalJson)
