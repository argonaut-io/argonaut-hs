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
    , toDouble
    , fromDouble
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
    , fromDoubleToNumberOrNull
    , fromDoubleToNumberOrString
    , jsonTrue
    , jsonFalse
    , jsonZero
    , jsonNull
    , emptyString
    , emptyArray
    , singleItemArray
    , emptyObject
    , singleItemObject
    , JObject(..)
    , JArray(..)
    , JString(..)
    , runJObject
    , runJArray
  ) where

import Control.Lens
import Control.Monad()
import Control.Applicative()
import Data.Maybe
import Data.Hashable(Hashable(..))
import qualified Data.List as L
import Data.Typeable(Typeable)
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as M
import Text.Printf
import qualified Data.Text as T

newtype JString = JString T.Text deriving (Show, Eq, Typeable)

instance Hashable JString where
  hashWithSalt salt (JString string) = hashWithSalt salt string

newtype JObject = JObject (M.HashMap JString Json) deriving (Show, Eq, Typeable)

runJObject :: JObject -> M.HashMap JString Json
runJObject (JObject fields) = fields

newtype JArray = JArray (V.Vector Json) deriving (Show, Eq, Typeable)

runJArray :: JArray -> V.Vector Json
runJArray (JArray values) = values

data Json = JsonObject !JObject
          | JsonArray !JArray
          | JsonString !JString
          | JsonNumber !Double
          | JsonBool !Bool
          | JsonNull
          deriving (Eq, Typeable)

instance Show Json where
  show (JsonObject (JObject !fields)) = ('{' : (L.concat $ L.intersperse "," $ fmap (\(!key, !value) -> (jsonStringShow key) ++ (':' : (show value))) $ M.toList fields)) ++ "}"
  show (JsonArray (JArray !entries)) = ('[' : (L.concat $ L.intersperse "," $ fmap show $ V.toList entries)) ++ "]"
  show (JsonString !string) = jsonStringShow string
  show (JsonNumber !number) = show number
  show (JsonBool !bool)     = if bool then "true" else "false"
  show JsonNull             = "null"

jsonStringShow :: JString -> String
jsonStringShow (JString !text) = '"' : (L.foldr escapeAndPrependChar "\"" $ T.unpack text)

escapeAndPrependChar :: Char -> String -> String
escapeAndPrependChar '\r' !string = '\\' : 'r' : string
escapeAndPrependChar '\n' !string = '\\' : 'n' : string
escapeAndPrependChar '\t' !string = '\\' : 't' : string
escapeAndPrependChar '\b' !string = '\\' : 'b' : string
escapeAndPrependChar '\f' !string = '\\' : 'f' : string
escapeAndPrependChar '\\' !string = '\\' : '\\' : string
escapeAndPrependChar '/' !string  = '\\' : '/' : string
escapeAndPrependChar '"' !string  = '\\' : '"' : string
escapeAndPrependChar !char !string
    | requiresEscaping  = (printf "\\u%04x" $ fromEnum char) ++ string
    | otherwise         = char : string
   where
    !requiresEscaping    = char < '\x20'

foldJson :: a -> (Bool -> a) -> (Double -> a) -> (JString -> a) -> (JArray -> a) -> (JObject -> a) -> Json -> a
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

foldJsonNumber :: a -> (Double -> a) -> Json -> a
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
toString (JsonString (JString text)) = Just $ T.unpack text
toString _ = Nothing

fromString :: String -> Json
fromString string = JsonString $ JString $ T.pack string

fromText :: T.Text -> Json
fromText text = JsonString $ JString text

toDouble :: Json -> Maybe Double
toDouble (JsonNumber double) = Just double
toDouble _ = Nothing

fromDouble :: Double -> Maybe Json
fromDouble double | isNaN double      = Nothing
                  | isInfinite double = Nothing
                  | otherwise         = Just (JsonNumber double)

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

numberL :: Lens Json (Maybe Json) (Maybe Double) Double
numberL = lens toDouble (const fromDouble)

arrayL :: Prism' Json JArray
arrayL = prism' fromArray toArray

objectL :: Prism' Json JObject
objectL = prism' fromObject toObject

stringL :: Prism' Json String
stringL = prism' fromString toString

nullL :: Prism' Json ()
nullL = prism' fromUnit toUnit

fromDoubleToNumberOrNull :: Double -> Json
fromDoubleToNumberOrNull = fromMaybe JsonNull . fromDouble

fromDoubleToNumberOrString :: Double -> Json
fromDoubleToNumberOrString double = fromMaybe (fromString $ show double) (fromDouble double)

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
emptyArray = JsonArray $ JArray $ V.empty

singleItemArray :: Json -> Json
singleItemArray = JsonArray . JArray . V.singleton

emptyObject :: Json
emptyObject = JsonObject $ JObject $ M.empty

singleItemObject :: JString -> Json -> Json
singleItemObject field json = JsonObject $ JObject $ (M.singleton field json)
