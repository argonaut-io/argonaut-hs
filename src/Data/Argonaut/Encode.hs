{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Argonaut.Encode
  (
      EncodeJson(..)
    , encodeTo
    , encodeIdentity
  ) where

import Data.Argonaut.Core
import Data.Maybe()
import Data.Scientific (Scientific)
import Control.Monad.Identity

class EncodeJson m n a where
  encodeJson :: m a -> n Json

encodeTo :: EncodeJson m n a => m a -> n Json
encodeTo = encodeJson

encodeIdentity :: EncodeJson Identity Identity a => a -> Json
encodeIdentity = runIdentity . encodeTo . Identity

instance EncodeJson Identity Identity JString where
  encodeJson = Identity . fromJString . runIdentity

instance EncodeJson Identity Identity Bool where
  encodeJson = Identity . fromBool . runIdentity

instance EncodeJson Identity Identity Scientific where
  encodeJson = Identity .fromScientific . runIdentity

instance EncodeJson Identity Identity JArray where
  encodeJson = Identity . fromArray . runIdentity

instance EncodeJson Identity Identity JObject where
  encodeJson = Identity . fromObject . runIdentity

instance EncodeJson Identity Identity Json where
  encodeJson = Identity . runIdentity

instance EncodeJson Identity Identity () where
  encodeJson _ = Identity $ fromUnit ()