module Data.Argonaut.Core
  ( Json(..)
  , JNull(..)
  , JBoolean(..)
  , JNumber(..)
  , JString(..)
  , JAssoc(..)
  , JArray(..)
  , JObject(..)
  , foldJson
  , arrayL
  , booleanL
  , foldJsonArray
  , foldJsonBoolean
  , foldJsonNull
  , foldJsonNumber
  , foldJsonObject
  , foldJsonString
  , fromArray
  , fromBoolean
  , fromNull
  , fromNumber
  , fromObject
  , fromString
  , isArray
  , isBoolean
  , isJsonType
  , isNull
  , isNumber
  , isObject
  , isString
  , jsonArrayL
  , jsonBooleanL
  , jsonEmptyArray
  , jsonEmptyObject
  , jsonEmptyString
  , jsonFalse
  , jsonNull
  , jsonNullL
  , jsonNumberL
  , jsonObjectL
  , jsonSingletonArray
  , jsonSingletonObject
  , jsonStringL
  , jsonTrue
  , jsonZero
  , nullL
  , numberL
  , objectL
  , stringL
  , toArray
  , toBoolean
  , toNull
  , toNumber
  , toObject
  , toString
  ) where

import Prelude

import Optic.Types (PrismP())
import Optic.Extended (TraversalP())
import Optic.Fold (filtered)
import Optic.Prism (prism')

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import qualified Data.StrMap as M
import Data.Function

type JBoolean = Boolean
type JNumber  = Number
type JString  = String
type JAssoc   = Tuple String Json
type JArray   = Array Json
type JObject  = M.StrMap Json

foreign import data JNull :: *
foreign import data Json :: *

-- Folds

foldJson :: forall a
            .  (JNull    -> a)
            -> (JBoolean -> a)
            -> (JNumber  -> a)
            -> (JString  -> a)
            -> (JArray   -> a)
            -> (JObject  -> a)
            -> Json
            -> a
foldJson a b c d e f json = runFn7 _foldJson a b c d e f json

foldJsonNull :: forall a. a -> (JNull -> a) -> Json -> a
foldJsonNull d f j = runFn7 _foldJson f (const d) (const d) (const d) (const d) (const d) j

foldJsonBoolean :: forall a. a -> (JBoolean -> a) -> Json -> a
foldJsonBoolean d f j = runFn7 _foldJson (const d) f (const d) (const d) (const d) (const d) j

foldJsonNumber :: forall a. a -> (JNumber -> a) -> Json -> a
foldJsonNumber d f j = runFn7 _foldJson (const d) (const d) f (const d) (const d) (const d) j

foldJsonString :: forall a. a -> (JString -> a) -> Json -> a
foldJsonString d f j = runFn7 _foldJson (const d) (const d) (const d) f (const d) (const d) j

foldJsonArray :: forall a. a -> (JArray -> a) -> Json -> a
foldJsonArray d f j = runFn7 _foldJson (const d) (const d) (const d) (const d) f (const d) j

foldJsonObject :: forall a. a -> (JObject -> a) -> Json -> a
foldJsonObject d f j = runFn7 _foldJson (const d) (const d) (const d) (const d) (const d) f j

verbJsonType :: forall a b
                .  b
                -> (a -> b)
                -> (b -> (a -> b) -> Json -> b)
                -> Json
                -> b
verbJsonType def f fold = fold def f

-- Tests

isJsonType :: forall a
              .  (Boolean -> (a -> Boolean) -> Json -> Boolean)
              -> Json
              -> Boolean
isJsonType = verbJsonType false (const true)

isNull    :: Json -> Boolean
isNull    = isJsonType foldJsonNull
isBoolean :: Json -> Boolean
isBoolean = isJsonType foldJsonBoolean
isNumber  :: Json -> Boolean
isNumber  = isJsonType foldJsonNumber
isString  :: Json -> Boolean
isString  = isJsonType foldJsonString
isArray   :: Json -> Boolean
isArray   = isJsonType foldJsonArray
isObject  :: Json -> Boolean
isObject  = isJsonType foldJsonObject


-- Decoding

toJsonType :: forall a b
              .  (Maybe a -> (a -> Maybe a) -> Json -> Maybe a)
              -> Json
              -> Maybe a
toJsonType = verbJsonType Nothing Just

toNull :: Json -> Maybe JNull
toNull = toJsonType foldJsonNull
toBoolean :: Json -> Maybe JBoolean
toBoolean = toJsonType foldJsonBoolean
toNumber :: Json -> Maybe JNumber
toNumber = toJsonType foldJsonNumber
toString :: Json -> Maybe JString
toString = toJsonType foldJsonString
toArray :: Json -> Maybe JArray
toArray = toJsonType foldJsonArray
toObject :: Json -> Maybe JObject
toObject = toJsonType foldJsonObject

-- Encoding

foreign import fromNull :: JNull -> Json
foreign import fromBoolean  :: JBoolean -> Json
foreign import fromNumber :: JNumber -> Json
foreign import fromString  :: JString -> Json
foreign import fromArray :: JArray -> Json
foreign import fromObject  :: JObject -> Json

-- Default values

jsonTrue :: Json
jsonTrue = fromBoolean true
jsonFalse :: Json
jsonFalse = fromBoolean false
jsonZero :: Json
jsonZero = fromNumber 0.0
foreign import jsonNull :: Json
jsonEmptyString :: Json
jsonEmptyString = fromString ""
jsonEmptyArray :: Json
jsonEmptyArray = fromArray []
jsonEmptyObject :: Json
jsonEmptyObject = fromObject M.empty
jsonSingletonArray :: Json -> Json
jsonSingletonArray j = fromArray [j]
jsonSingletonObject :: String -> Json -> Json
jsonSingletonObject key val = fromObject $ M.singleton key val

-- Prisms

nullL :: PrismP Json JNull
nullL = prism' fromNull toNull
booleanL :: PrismP Json JBoolean
booleanL = prism' fromBoolean toBoolean
numberL :: PrismP Json JNumber
numberL = prism' fromNumber toNumber
stringL :: PrismP Json JString
stringL = prism' fromString toString
arrayL :: PrismP Json JArray
arrayL = prism' fromArray toArray
objectL :: PrismP Json JObject
objectL = prism' fromObject toObject

-- Traversals

jsonNullL :: TraversalP Json Json
jsonNullL = id <<< filtered isNull
jsonBooleanL :: TraversalP Json Json
jsonBooleanL = id <<< filtered isBoolean
jsonNumberL :: TraversalP Json Json
jsonNumberL = id <<< filtered isNumber
jsonStringL :: TraversalP Json Json
jsonStringL = id <<< filtered isString
jsonArrayL :: TraversalP Json Json
jsonArrayL = id <<< filtered isArray
jsonObjectL :: TraversalP Json Json
jsonObjectL = id <<< filtered isObject

instance eqJNull :: Eq JNull where
  eq n1 n2 = true


instance ordJNull :: Ord JNull where
  compare = const <<< const EQ

instance showJson :: Show Json where
  show = _stringify

instance showJsonNull :: Show JNull where
  show = const "null"

instance eqJson :: Eq Json where
  eq j1 j2 = (compare j1 j2) == EQ

instance ordJson :: Ord Json where
  compare a b = runFn5 _compare EQ GT LT a b

foreign import _stringify :: Json -> String

foreign import _foldJson :: forall z.
                            Fn7 (JNull -> z)
                            (JBoolean -> z)
                            (JNumber -> z)
                            (JString -> z)
                            (JArray -> z)
                            (JObject -> z)
                            Json z

 -- very fast ordering for Json
foreign import _compare :: Fn5 Ordering Ordering Ordering Json Json Ordering
