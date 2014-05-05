{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Data.Argonaut.CoreSpec where

import qualified Data.Text as T
import Data.Argonaut
import Data.Argonaut.TestInstances()
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

foldFromToIsTest :: (Arbitrary a, Eq a, Show a) => String -> (Int -> (b -> Int) -> Json -> Int) -> (a -> Json) -> (Json -> Maybe a) -> (Json -> Bool) -> Spec
foldFromToIsTest name foldX fromX toX isX = prop name $ do
  (\fromValue -> \foldDefault -> \foldResult -> \json -> 
    let
      fromToLaw  = (toX $ fromX fromValue) `shouldBe` Just fromValue
      foldLaw    = foldX foldDefault (\_ -> foldResult) json `shouldBe` if isX json then foldResult else foldDefault
      fromIsLaw  = (isX $ fromX fromValue) `shouldBe` True
    in fromToLaw .&&. foldLaw .&&. fromIsLaw)

spec :: Spec
spec = do
  describe "foldX, fromX, toX, isX methods" $ do
    foldFromToIsTest "JsonNull" foldJsonNull fromUnit toUnit isNull
    foldFromToIsTest "JsonBool" foldJsonBool fromBool toBool isBool
    foldFromToIsTest "JsonNumber" foldJsonNumber fromScientific toScientific isNumber
    foldFromToIsTest "JsonString" foldJsonString fromJString toJString isString
    foldFromToIsTest "JsonArray" foldJsonArray fromArray toArray isArray
    foldFromToIsTest "JsonObject" foldJsonObject fromObject toObject isObject
