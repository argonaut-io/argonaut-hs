{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Data.Argonaut.Decode
  (
      DecodeJson(..)
    , decodeFrom
    , decodeMaybe
    , EitherStringDecodeResult
    , decodeL
    , objectFieldL
    , arrayIndexL
    , objectMembersL
    , arrayMembersL
  ) where

import Data.Argonaut.Core
import Data.Argonaut.Encode
import Data.Maybe()
import Data.Scientific (Scientific)
import Control.Lens
import Control.Monad.Identity()
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as M

type EitherStringDecodeResult = Either String

class DecodeJson m n a | m a -> n where
  decodeJson :: m Json -> n a

decodeFrom :: DecodeJson m n a => m Json -> n a
decodeFrom = decodeJson

decodeMaybe :: DecodeJson Identity EitherStringDecodeResult a => Json -> Maybe a
decodeMaybe = either (const Nothing) Just . decodeFrom . Identity

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
  decodeJson = foldJson (\_ -> valid) (\_ -> invalid) (\_ -> invalid) (\_ -> invalid) validArray validObject . runIdentity
    where valid = Right ()
          invalid = Left "Not an empty value."
          validArray array = if V.null array then valid else invalid
          validObject object = if M.null object then valid else valid

decodeL :: (DecodeJson Identity EitherStringDecodeResult a, EncodeJson Identity Identity a) => Prism' Json a
decodeL = prism' encodeIdentity decodeMaybe

objectFieldL :: (DecodeJson Identity EitherStringDecodeResult a, EncodeJson Identity Identity a) => JString -> Traversal' Json a
objectFieldL name = objectL . ix name . decodeL

arrayIndexL :: (DecodeJson Identity EitherStringDecodeResult a, EncodeJson Identity Identity a) => Int -> Traversal' Json a
arrayIndexL arrayIndex = arrayL . ix arrayIndex . decodeL

objectMembersL :: (DecodeJson Identity EitherStringDecodeResult a, EncodeJson Identity Identity a) => IndexedTraversal' JString Json a
objectMembersL = objectL . itraversed . decodeL

arrayMembersL :: (DecodeJson Identity EitherStringDecodeResult a, EncodeJson Identity Identity a) => IndexedTraversal' Int Json a
arrayMembersL = arrayL . traversed . decodeL
