{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}

module Data.Argonaut.TestInstances where

import Control.Monad
import Data.Argonaut
import Test.QuickCheck
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as M

instance Arbitrary Json where
  arbitrary = genJsonDepthLimited 3

instance Arbitrary JArray where
  arbitrary = liftM V.fromList arbitrary

instance Arbitrary JObject where
  arbitrary = liftM M.fromList arbitrary

genJsonObject :: Gen Json
genJsonObject = liftM (fromObject . M.fromList) arbitrary

genJsonArray :: Gen Json
genJsonArray = liftM (fromArray . V.fromList) arbitrary

genJsonNumber :: Gen Json
genJsonNumber = liftM fromDoubleToNumberOrNull arbitrary

genJsonString :: Gen Json
genJsonString = liftM fromString arbitrary

genJsonBool :: Gen Json
genJsonBool = liftM fromBool arbitrary

genJsonNull :: Gen Json
genJsonNull = return jsonNull

genNonNestedJson :: Gen Json
genNonNestedJson = frequency [
                    (5, genJsonNumber)
                    , (5, genJsonString)
                    , (2, genJsonBool)
                    , (1, genJsonNull)
                   ]

genJsonDepthLimited :: Int -> Gen Json
genJsonDepthLimited n | n > 1     = frequency [
                                            (1, (liftM (fromObject . M.fromList) (genJsonFieldListDepthLimited (n - 1))))
                                            , (1, (liftM (fromArray . V.fromList) (genJsonListDepthLimited (n - 1))))
                                            , (8, genNonNestedJson)
                                          ]
                      | otherwise = genNonNestedJson

genJsonListDepthLimited :: Int -> Gen [Json]
genJsonListDepthLimited n = listOf (genJsonDepthLimited n)

genJsonFieldListDepthLimited :: Int -> Gen [(JField, Json)]
genJsonFieldListDepthLimited n =
  let generator = do  key <- arbitrary
                      value <- genJsonDepthLimited n
                      return (key, value)
  in listOf generator
