{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Data.Argonaut.TestTemplates where

import Control.Monad.Identity
import Data.Argonaut
import Data.Argonaut.Encode
import Data.Argonaut.Decode
import Data.Traversable
import Test.Hspec
import Test.QuickCheck
import Language.Haskell.TH

buildEncodeDecodeTest :: [Name] -> Q Exp
buildEncodeDecodeTest names = join $ fmap (\doStatements -> [|describe "encode/decode" $ $(doE $ fmap (noBindS . return) doStatements)|]) $
  traverse (\typeName -> [|
    it ("for a " ++ $(stringE $ show typeName) ++ " decoding should mirror encoding") $ do
      property $ \valueOfType ->
        let
          liftAndEncode = (encode :: Identity $(conT typeName) -> EitherStringEncodeResult Json) . return
          liftAndDecode = (decode :: Identity Json -> EitherStringDecodeResult $(conT typeName)) . return
          encodeThenDecode valueBeforeEncoding = liftAndEncode valueBeforeEncoding >>= liftAndDecode
        in ((encodeThenDecode valueOfType) `shouldBe` (return valueOfType))
    |]) names
