{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, BangPatterns #-}

module Data.Argonaut
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
    , boolL
    , numberL
    , arrayL
    , objectL
    , stringL
    , nullL
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

foldJson :: a -> (Bool -> a) -> (Scientific -> a) -> (JString -> a) -> (JArray -> a) -> (JObject -> a) -> Json -> a
foldJson _ _ _ _ _ jsonObject (JsonObject value)  = jsonObject value
foldJson _ _ _ _ jsonArray _ (JsonArray value)    = jsonArray value
foldJson _ _ _ jsonString _ _ (JsonString value)  = jsonString value
foldJson _ _ jsonNumber _ _ _ (JsonNumber value)  = jsonNumber value
foldJson _ jsonBool _ _ _ _ (JsonBool value)      = jsonBool value
foldJson jsonNullValue _ _ _ _ _ (JsonNull)       = jsonNullValue

foldJsonNull :: a -> a -> Json -> a
foldJsonNull _ nullValue JsonNull = nullValue
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
isNull JsonNull = True
isNull _ = False

isTrue :: Json -> Bool
isTrue (JsonBool True) = True
isTrue _ = False

isFalse :: Json -> Bool
isFalse (JsonBool False) = True
isFalse _ = False

isNumber :: Json -> Bool
isNumber (JsonNumber _) = True
isNumber _ = False

isString :: Json -> Bool
isString (JsonString _) = True
isString _ = False

isArray :: Json -> Bool
isArray (JsonArray _) = True
isArray _ = False

isObject :: Json -> Bool
isObject (JsonObject _) = True
isObject _ = False

toBool :: Json -> Maybe Bool
toBool (JsonBool bool) = Just bool
toBool _ = Nothing

fromBool :: Bool -> Json
fromBool = JsonBool

toJString :: Json -> Maybe JString
toJString (JsonString text) = Just text
toJString _ = Nothing

fromJString :: JString -> Json
fromJString string = JsonString $ string

toString :: Json -> Maybe String
toString (JsonString text) = Just $ T.unpack text
toString _ = Nothing

fromString :: String -> Json
fromString string = JsonString $ T.pack string

fromText :: T.Text -> Json
fromText text = JsonString text

toScientific :: Json -> Maybe Scientific
toScientific (JsonNumber scientific) = Just scientific
toScientific _ = Nothing

fromScientific :: Scientific -> Json
fromScientific scientific = JsonNumber scientific

toArray :: Json -> Maybe JArray
toArray (JsonArray array) = Just array
toArray _ = Nothing

fromArray :: JArray -> Json
fromArray = JsonArray

toObject :: Json -> Maybe JObject
toObject (JsonObject object) = Just object
toObject _ = Nothing

fromObject :: JObject -> Json
fromObject = JsonObject

toUnit :: Json -> Maybe ()
toUnit JsonNull = Just ()
toUnit _ = Nothing

fromUnit :: () -> Json
fromUnit _ = JsonNull

boolL :: Prism' Json Bool
boolL = prism' fromBool toBool

numberL :: Lens Json Json (Maybe Scientific) Scientific
numberL = lens toScientific (const fromScientific)

arrayL :: Prism' Json JArray
arrayL = prism' fromArray toArray

objectL :: Prism' Json JObject
objectL = prism' fromObject toObject

stringL :: Prism' Json String
stringL = prism' fromString toString

nullL :: Prism' Json ()
nullL = prism' fromUnit toUnit

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
