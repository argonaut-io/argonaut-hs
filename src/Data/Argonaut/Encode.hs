{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, BangPatterns, MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances, FlexibleContexts #-}

module Data.Argonaut.Encode
  (
      EncodeJson(..)
    , encode
  ) where

import Data.Argonaut
import Data.Maybe()
import Control.Monad
import Control.Monad.Identity

class EncodeJson m n a where
  encodeJson :: m a -> n Json

encode :: EncodeJson m n a => m a -> n Json
encode = encodeJson

type EitherStringEncodeResult = Either String

instance EncodeJson Identity Identity a => EncodeJson Identity EitherStringEncodeResult a where
  encodeJson = fmap (Right . runIdentity) encodeJson

instance EncodeJson Identity Identity String where
  encodeJson = liftM fromString

instance EncodeJson Identity Identity Bool where
  encodeJson = liftM fromBool

instance EncodeJson Identity EitherStringEncodeResult Double where
  encodeJson = fmap (maybe (Left "Invalid Double value.") Right) (fromDouble . runIdentity)

instance EncodeJson Identity Identity JArray where
  encodeJson = liftM fromArray

instance EncodeJson Identity Identity JObject where
  encodeJson = liftM fromObject

instance EncodeJson Identity Identity Json where
  encodeJson = id
