{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Argonaut.TestTemplates where

import Control.Monad.Identity
import Data.Argonaut
import Data.Traversable
import Test.Hspec
import Test.QuickCheck
import Language.Haskell.TH

buildEncodeDecodeTest :: [Name] -> Q Exp
buildEncodeDecodeTest names = join $ fmap (\doStatements -> [|describe "encode/decode" $ $(doE $ fmap (noBindS . return) doStatements)|]) $
  traverse (\typeName -> [|
    it ($(stringE $ show typeName) ++ " decoding should mirror encoding") $ do
      property $ \valueOfType ->
        let
          liftAndEncode = (encodeTo :: Identity $(conT typeName) -> Identity Json) . return
          liftAndDecode = (decodeFrom :: Identity Json -> EitherStringDecodeResult $(conT typeName))
          encodeThenDecode = liftAndDecode . liftAndEncode
        in ((encodeThenDecode valueOfType) `shouldBe` (return valueOfType))
    |]) names
