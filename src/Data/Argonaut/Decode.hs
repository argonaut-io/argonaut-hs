{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, BangPatterns, MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances, FlexibleContexts #-}

module Data.Argonaut.Decode
  (
      DecodeJson(..)
    , decode
    , EitherStringDecodeResult
  ) where

import Data.Traversable
import Data.Argonaut.Core
import Data.Maybe()
import Data.Scientific (Scientific)
import Control.Monad.Identity
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as M

class DecodeJson m n a where
  decodeJson :: m Json -> n a

decode :: DecodeJson m n a => m Json -> n a
decode = decodeJson

type EitherStringDecodeResult = Either String

instance DecodeJson Identity EitherStringDecodeResult JString where
  decodeJson = foldJsonString (Left "Not a String.") Right . runIdentity

instance DecodeJson Identity EitherStringDecodeResult Bool where
  decodeJson = foldJsonBool (Left "Not a Bool.") Right . runIdentity

instance DecodeJson Identity EitherStringDecodeResult Scientific where
  decodeJson = foldJsonNumber (Left "Not a Number.") Right . runIdentity

instance DecodeJson Identity EitherStringDecodeResult JArray where
  decodeJson = foldJsonArray (Left "Not an Array.") Right . runIdentity

instance DecodeJson Identity EitherStringDecodeResult JObject where
  decodeJson = foldJsonObject (Left "Not an Object.") Right . runIdentity

instance DecodeJson Identity EitherStringDecodeResult Json where
  decodeJson = Right . runIdentity

instance DecodeJson Identity EitherStringDecodeResult () where
  decodeJson = foldJson valid (\_ -> invalid) (\_ -> invalid) (\_ -> invalid) validArray validObject . runIdentity
    where valid = Right ()
          invalid = Left "Not an empty value."
          validArray array = if V.null array then valid else invalid
          validObject object = if M.null object then valid else valid