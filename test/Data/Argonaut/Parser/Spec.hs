{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Data.Argonaut.Parser.Spec where

import Debug.Trace
import Control.Monad
import Data.Argonaut
import Data.Argonaut.Parser
import Test.Hspec
import Test.QuickCheck
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as M

instance Arbitrary Json where
  arbitrary = oneof [
                  liftM (fromObject . M.fromList) arbitrary
                , liftM (fromArray . V.fromList) arbitrary
                , liftM fromDoubleToNumberOrNull arbitrary
                , liftM fromString arbitrary
                , liftM fromBool arbitrary
                , return jsonNull
              ]

spec :: Spec
spec = do
  describe "parseString" $ do
    it "for some valid json, produces the same value" $ do
      property $ \originalJson ->
        let
          asString = show originalJson
          _ = trace asString
          _ = trace "Testing!"
          parsedJson = parseString asString
        in parsedJson `shouldBe` (Right originalJson)
