{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}

module Data.Argonaut.Core
  (
      Json
    , foldJson
    , foldJsonNull
    , foldJsonBool
    , foldJsonNumber
    , foldJsonString
    , foldJsonArray
    , foldJsonObject
    , isNull
    , isBool
    , isTrue
    , isFalse
    , isNumber
    , isString
    , isArray
    , isObject
    , toBool
    , fromBool
    , toJString
    , fromJString
    , toString
    , fromString
    , fromText
    , toScientific
    , fromScientific
    , toArray
    , fromArray
    , toObject
    , fromObject
    , toUnit
    , fromUnit
    , jsonTrue
    , jsonFalse
    , jsonZero
    , jsonNull
    , emptyString
    , emptyArray
    , singleItemArray
    , emptyObject
    , singleItemObject
    , JObject
    , JArray
    , JString
    , boolL
    , jsonBoolL
    , numberL
    , jsonNumberL
    , arrayL
    , jsonArrayL
    , objectL
    , jsonObjectL
    , stringL
    , jsonStringL
    , nullL
    , jsonNullL
  ) where

import Control.Lens
import Control.Monad()
import Control.Applicative()
import Data.Scientific (Scientific)
import Data.Typeable(Typeable)
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as M
import qualified Data.Text as T

type JObject = M.HashMap JString Json

type JArray = V.Vector Json

type JString = T.Text

data Json = JsonObject !JObject
          | JsonArray !JArray
          | JsonString !JString
          | JsonNumber !Scientific
          | JsonBool !Bool
          | JsonNull
          deriving (Eq, Typeable)

foldJson :: (() -> a) -> (Bool -> a) -> (Scientific -> a) -> (JString -> a) -> (JArray -> a) -> (JObject -> a) -> Json -> a
foldJson _ _ _ _ _ jsonObject (JsonObject value)  = jsonObject value
foldJson _ _ _ _ jsonArray _ (JsonArray value)    = jsonArray value
foldJson _ _ _ jsonString _ _ (JsonString value)  = jsonString value
foldJson _ _ jsonNumber _ _ _ (JsonNumber value)  = jsonNumber value
foldJson _ jsonBool _ _ _ _ (JsonBool value)      = jsonBool value
foldJson jsonNullTransform _ _ _ _ _ (JsonNull)   = jsonNullTransform ()

foldJsonNull :: a -> (() -> a) -> Json -> a
foldJsonNull _ nullTransform JsonNull = nullTransform ()
foldJsonNull defaultValue _ _ = defaultValue

foldJsonBool :: a -> (Bool -> a) -> Json -> a
foldJsonBool _ valueTransform (JsonBool value) = valueTransform value
foldJsonBool defaultValue _ _ = defaultValue

foldJsonNumber :: a -> (Scientific -> a) -> Json -> a
foldJsonNumber _ valueTransform (JsonNumber value) = valueTransform value
foldJsonNumber defaultValue _ _ = defaultValue

foldJsonString :: a -> (JString -> a) -> Json -> a
foldJsonString _ valueTransform (JsonString value) = valueTransform value
foldJsonString defaultValue _ _ = defaultValue

foldJsonArray :: a -> (JArray -> a) -> Json -> a
foldJsonArray _ valueTransform (JsonArray value) = valueTransform value
foldJsonArray defaultValue _ _ = defaultValue

foldJsonObject :: a -> (JObject -> a) -> Json -> a
foldJsonObject _ valueTransform (JsonObject value) = valueTransform value
foldJsonObject defaultValue _ _ = defaultValue

isNull :: Json -> Bool
isNull = foldJsonNull False (\_ -> True)

isBool :: Json -> Bool
isBool = foldJsonBool False (\_ -> True)

isTrue :: Json -> Bool
isTrue = foldJsonBool False id

isFalse :: Json -> Bool
isFalse = foldJsonBool False not

isNumber :: Json -> Bool
isNumber = foldJsonNumber False (\_ -> True)

isString :: Json -> Bool
isString = foldJsonString False (\_ -> True)

isArray :: Json -> Bool
isArray = foldJsonArray False (\_ -> True)

isObject :: Json -> Bool
isObject = foldJsonObject False (\_ -> True)

toBool :: Json -> Maybe Bool
toBool = foldJsonBool Nothing Just

fromBool :: Bool -> Json
fromBool = JsonBool

toJString :: Json -> Maybe JString
toJString = foldJsonString Nothing Just

fromJString :: JString -> Json
fromJString string = JsonString $ string

toString :: Json -> Maybe String
toString = fmap T.unpack . toJString

fromString :: String -> Json
fromString string = JsonString $ T.pack string

fromText :: T.Text -> Json
fromText text = JsonString text

toScientific :: Json -> Maybe Scientific
toScientific = foldJsonNumber Nothing Just

fromScientific :: Scientific -> Json
fromScientific scientific = JsonNumber scientific

toArray :: Json -> Maybe JArray
toArray = foldJsonArray Nothing Just

fromArray :: JArray -> Json
fromArray = JsonArray

toObject :: Json -> Maybe JObject
toObject = foldJsonObject Nothing Just

fromObject :: JObject -> Json
fromObject = JsonObject

toUnit :: Json -> Maybe ()
toUnit JsonNull = Just ()
toUnit _ = Nothing

fromUnit :: () -> Json
fromUnit _ = JsonNull

jsonTrue :: Json
jsonTrue = JsonBool True

jsonFalse :: Json
jsonFalse = JsonBool False

jsonZero :: Json
jsonZero = JsonNumber 0

jsonNull :: Json
jsonNull = JsonNull

emptyString :: Json
emptyString = fromString ""

emptyArray :: Json
emptyArray = JsonArray $ V.empty

singleItemArray :: Json -> Json
singleItemArray = JsonArray . V.singleton

emptyObject :: Json
emptyObject = JsonObject $ M.empty

singleItemObject :: JString -> Json -> Json
singleItemObject field json = JsonObject $ M.singleton field json

boolL :: Prism' Json Bool
boolL = prism' fromBool toBool

jsonBoolL :: Traversal' Json Json
jsonBoolL = id . filtered isBool

numberL :: Prism' Json Scientific 
numberL = prism' fromScientific toScientific

jsonNumberL :: Traversal' Json Json
jsonNumberL = id . filtered isNumber

arrayL :: Prism' Json JArray
arrayL = prism' fromArray toArray

jsonArrayL :: Traversal' Json Json
jsonArrayL = id . filtered isArray

objectL :: Prism' Json JObject
objectL = prism' fromObject toObject

jsonObjectL :: Traversal' Json Json
jsonObjectL = id . filtered isObject

stringL :: Prism' Json String
stringL = prism' fromString toString

jsonStringL :: Traversal' Json Json
jsonStringL = id . filtered isString

nullL :: Prism' Json ()
nullL = prism' fromUnit toUnit

jsonNullL :: Traversal' Json Json
jsonNullL = id . filtered isNull
