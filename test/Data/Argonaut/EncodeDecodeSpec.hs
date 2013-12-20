{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Data.Argonaut.EncodeDecodeSpec where

import Control.Monad.Identity
import Data.Argonaut
import Data.Argonaut.Encode
import Data.Argonaut.Decode
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  describe "encode/decode" $ do
    it ("for a string decoding should mirror encoding") $ do
      property $ \valueOfType ->
        let
          liftAndEncode = (encode :: Identity String -> EitherStringEncodeResult Json) . return
          liftAndDecode = (decode :: Identity Json -> EitherStringDecodeResult String) . return
          encodeThenDecode valueBeforeEncoding = liftAndEncode valueBeforeEncoding >>= liftAndDecode
        in ((encodeThenDecode valueOfType) `shouldBe` (return valueOfType))
