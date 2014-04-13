{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, BangPatterns, MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances, FlexibleContexts #-}

module Data.Argonaut.Encode
  (
      EncodeJson(..)
    , encode
    , EitherStringEncodeResult
  ) where

import Data.Traversable
import Data.Argonaut
import Data.Maybe()
import Data.Scientific (Scientific)
import Control.Monad.Identity
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as M

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

{-
instance EncodeJson Identity EitherStringEncodeResult a => EncodeJson Identity EitherStringEncodeResult [a] where
  encodeJson = fmap (fromArray . V.fromList) . (traverse (encode . Identity)) . runIdentity

instance EncodeJson Identity EitherStringEncodeResult a => EncodeJson Identity EitherStringEncodeResult (M.HashMap JString a) where
  encodeJson = fmap (fromObject) . (traverse (encode . Identity)) . runIdentity

instance EncodeJson Identity EitherStringEncodeResult a => EncodeJson Identity EitherStringEncodeResult (V.Vector a) where
  encodeJson = fmap (fromArray) . (traverse (encode . Identity)) . runIdentity
-}