{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, BangPatterns, MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances, FlexibleContexts #-}

module Data.Argonaut.Encode
  (
      EncodeJson(..)
    , encode
    , EitherStringEncodeResult
  ) where

import Data.Argonaut.Core
import Data.Maybe()
import Data.Scientific (Scientific)
import Control.Monad.Identity

class EncodeJson m n a where
  encodeJson :: m a -> n Json

encode :: EncodeJson m n a => m a -> n Json
encode = encodeJson

type EitherStringEncodeResult = Either String

instance EncodeJson Identity EitherStringEncodeResult JString where
  encodeJson = Right . fromJString . runIdentity

instance EncodeJson Identity EitherStringEncodeResult Bool where
  encodeJson = Right . fromBool . runIdentity

instance EncodeJson Identity EitherStringEncodeResult Scientific where
  encodeJson = Right .fromScientific . runIdentity

instance EncodeJson Identity EitherStringEncodeResult JArray where
  encodeJson = Right . fromArray . runIdentity

instance EncodeJson Identity EitherStringEncodeResult JObject where
  encodeJson = Right . fromObject . runIdentity

instance EncodeJson Identity EitherStringEncodeResult Json where
  encodeJson = Right . runIdentity

instance EncodeJson Identity EitherStringEncodeResult () where
  encodeJson _ = Right emptyObject