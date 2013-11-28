{-# LANGUAGE DeriveDataTypeable #-}

module Data.Argonaut
  (
      Json
    , foldJson
    , isNull
    , isTrue
    , isFalse
    , isNumber
    , isString
    , isArray
    , isObject
    , toBool
    , fromBool
    , toText
    , fromText
    , toString
    , fromString
    , toDouble
    , fromDouble
    , toArray
    , fromArray
    , toObject
    , fromObject
    , jBoolP
    , jNumberL
  ) where

import Control.Lens
import Control.Monad()
import Control.Applicative()
import Data.Text(Text,unpack,pack)
import Data.Typeable(Typeable)
import Data.Vector(Vector)
import Data.HashMap.Strict(HashMap)

type JObject = HashMap Text Json

type JArray = Vector Json

data Json = JsonObject !JObject
          | JsonArray !JArray
          | JsonString !Text
          | JsonNumber !Double
          | JsonBool !Bool
          | JsonNull
          deriving (Eq, Show, Typeable)

foldJson :: a -> (Bool -> a) -> (Double -> a) -> (Text -> a) -> (JArray -> a) -> (JObject -> a) -> Json -> a
foldJson _ _ _ _ _ jsonObject (JsonObject value) = jsonObject value
foldJson _ _ _ _ jsonArray _ (JsonArray value) = jsonArray value
foldJson _ _ _ jsonString _ _ (JsonString value) = jsonString value
foldJson _ _ jsonNumber _ _ _ (JsonNumber value) = jsonNumber value
foldJson _ jsonBool _ _ _ _ (JsonBool value) = jsonBool value
foldJson jsonNull _ _ _ _ _ (JsonNull) = jsonNull

isNull :: Json => Bool
isNull JsonNull = True
isNull _ = False

isTrue :: Json => Bool
isTrue (JsonBool True) = True
isTrue _ = False

isFalse :: Json => Bool
isFalse (JsonBool False) = True
isFalse _ = False

isNumber :: Json => Bool
isNumber (JsonNumber _) = True
isNumber _ = False

isString :: Json => Bool
isString (JsonString _) = True
isString _ = False

isArray :: Json => Bool
isArray (JsonArray _) = True
isArray _ = False

isObject :: Json => Bool
isObject (JsonObject _) = True
isObject _ = False

toBool :: Json => Maybe Bool
toBool (JsonBool bool) = Just bool
toBool _ = Nothing

fromBool :: Bool => Json
fromBool = JsonBool

toText :: Json => Maybe Text
toText (JsonString text) = Just text
toText _ = Nothing

fromText :: Text => Json
fromText = JsonString

toString :: Json => Maybe String
toString (JsonString text) = Just (unpack text)
toString _ = Nothing

fromString :: String => Json
fromString string = JsonString (pack string)

toDouble :: Json => Maybe Double
toDouble (JsonNumber double) = Just double
toDouble _ = Nothing

fromDouble :: Double => Maybe Json
fromDouble double | isNaN double      = Nothing
                  | isInfinite double = Nothing
                  | otherwise         = Just (JsonNumber double)

toArray :: Json => Maybe JArray
toArray (JsonArray array) = Just array
toArray _ = Nothing

fromArray :: JArray => Json
fromArray = JsonArray

toObject :: Json => Maybe JObject
toObject (JsonObject object) = Just object
toObject _ = Nothing

fromObject :: JObject => Json
fromObject = JsonObject

jBoolP :: Prism' Json Bool
jBoolP = prism' fromBool toBool

jNumberL :: Lens Json (Maybe Json) (Maybe Double) Double
jNumberL = lens toDouble (const fromDouble)
