{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Data.Argonaut.ParserSpec where

import qualified Data.Text as T
import Data.Argonaut
import Data.Argonaut.TestInstances()
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  describe "parseText" $ do
    it "for some valid json, produces the same value" $ do
      property $ \originalJson ->
        let
          asString = show originalJson
          parsedJson = parseText $ T.pack asString
        in parsedJson `shouldBe` (ParseSuccess originalJson)
