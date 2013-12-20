{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, BangPatterns, MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances, FlexibleContexts #-}

module Data.Argonaut.Decode
  (
      DecodeJson(..)
    , decode
    , EitherStringDecodeResult
  ) where

import Data.Argonaut
import Data.Maybe()
--import Control.Monad
import Control.Monad.Identity

class DecodeJson m n a where
  decodeJson :: m Json -> n a

decode :: DecodeJson m n a => m Json -> n a
decode = decodeJson

type EitherStringDecodeResult = Either String

instance DecodeJson Identity EitherStringDecodeResult String where
  decodeJson = foldJsonString (Left "Not a String.") Right . runIdentity

instance DecodeJson Identity EitherStringDecodeResult Bool where
  decodeJson = foldJsonBool (Left "Not a Bool.") Right . runIdentity

instance DecodeJson Identity EitherStringDecodeResult Double where
  decodeJson = foldJsonNumber (Left "Not a Number.") Right . runIdentity

instance DecodeJson Identity EitherStringDecodeResult JArray where
  decodeJson = foldJsonArray (Left "Not an Array.") Right . runIdentity

instance DecodeJson Identity EitherStringDecodeResult JObject where
  decodeJson = foldJsonObject (Left "Not an Object.") Right . runIdentity

instance DecodeJson Identity EitherStringDecodeResult Json where
  decodeJson = Right . runIdentity
