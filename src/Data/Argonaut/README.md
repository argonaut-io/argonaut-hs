# Module Documentation

## Module Data.Argonaut.Combinators

### Values

    (:=) :: forall a. (EncodeJson a) => String -> a -> JAssoc

    (?>>=) :: forall a b. Maybe a -> String -> Either String a

    (~>) :: JAssoc -> Json -> Json


## Module Data.Argonaut.Core

### Types

    type JArray  = [Json]

    type JAssoc  = Tuple JField Json

    type JBoolean  = Boolean

    type JField  = String

    data JNull :: *

    type JNumber  = Number

    type JObject  = M.StrMap Json

    type JString  = String

    data Json :: *


### Type Class Instances

    instance eqJNull :: Eq JNull

    instance eqJson :: Eq Json

    instance showJson :: Show Json

    instance showJsonNull :: Show JNull


### Values

    _foldJson :: forall z. Fn7 (JNull -> z) (JBoolean -> z) (JNumber -> z) (JString -> z) (JArray -> z) (JObject -> z) Json z

    _stringify :: Json -> String

    arrayL :: PrismP Json JArray

    booleanL :: PrismP Json JBoolean

    foldJson :: forall a. (JNull -> a) -> (JBoolean -> a) -> (JNumber -> a) -> (JString -> a) -> (JArray -> a) -> (JObject -> a) -> Json -> a

    foldJsonArray :: forall a. a -> (JArray -> a) -> Json -> a

    foldJsonBoolean :: forall a. a -> (JBoolean -> a) -> Json -> a

    foldJsonNull :: forall a. a -> (JNull -> a) -> Json -> a

    foldJsonNumber :: forall a. a -> (JNumber -> a) -> Json -> a

    foldJsonObject :: forall a. a -> (JObject -> a) -> Json -> a

    foldJsonString :: forall a. a -> (JString -> a) -> Json -> a

    fromArray :: JArray -> Json

    fromBoolean :: JBoolean -> Json

    fromNull :: JNull -> Json

    fromNumber :: JNumber -> Json

    fromObject :: JObject -> Json

    fromString :: JString -> Json

    isArray :: Json -> Boolean

    isBoolean :: Json -> Boolean

    isJsonType :: forall a. (Boolean -> (a -> Boolean) -> Json -> Boolean) -> Json -> Boolean

    isNull :: Json -> Boolean

    isNumber :: Json -> Boolean

    isObject :: Json -> Boolean

    isString :: Json -> Boolean

    jsonArrayL :: TraversalP Json Json

    jsonBooleanL :: TraversalP Json Json

    jsonEmptyArray :: Json

    jsonEmptyObject :: Json

    jsonEmptyString :: Json

    jsonFalse :: Json

    jsonNull :: Json

    jsonNullL :: TraversalP Json Json

    jsonNumberL :: TraversalP Json Json

    jsonObjectL :: TraversalP Json Json

    jsonSingletonArray :: Json -> Json

    jsonSingletonObject :: JField -> Json -> Json

    jsonStringL :: TraversalP Json Json

    jsonTrue :: Json

    jsonZero :: Json

    nullL :: PrismP Json JNull

    numberL :: PrismP Json JNumber

    objectL :: PrismP Json JObject

    stringL :: PrismP Json JString

    toArray :: Json -> Maybe JArray

    toBoolean :: Json -> Maybe JBoolean

    toJsonType :: forall a b. (Maybe a -> (a -> Maybe a) -> Json -> Maybe a) -> Json -> Maybe a

    toNull :: Json -> Maybe JNull

    toNumber :: Json -> Maybe JNumber

    toObject :: Json -> Maybe JObject

    toString :: Json -> Maybe JString

    verbJsonType :: forall a b. b -> (a -> b) -> (b -> (a -> b) -> Json -> b) -> Json -> b


## Module Data.Argonaut.Decode

### Type Classes

    class DecodeJson a where
      decodeJson :: Json -> Either String a


### Type Class Instances

    instance decodeArray :: (DecodeJson a) => DecodeJson [a]

    instance decodeJsonArray :: DecodeJson [Json]

    instance decodeJsonBoolean :: DecodeJson Boolean

    instance decodeJsonJson :: DecodeJson Json

    instance decodeJsonNull :: DecodeJson Unit

    instance decodeJsonNumber :: DecodeJson Number

    instance decodeJsonString :: DecodeJson String

    instance decodeMap :: (DecodeJson a) => DecodeJson (M.StrMap a)

    instance traversableMap :: Traversable M.StrMap


### Values

    arrayIndexL :: forall a. (DecodeJson a, EncodeJson a) => JNumber -> TraversalP Json a

    decodeL :: forall a. (DecodeJson a, EncodeJson a) => PrismP Json a

    decodeMaybe :: forall a. (DecodeJson a) => Json -> Maybe a

    objectFieldL :: forall a. (DecodeJson a, EncodeJson a) => JString -> TraversalP Json a


## Module Data.Argonaut.Encode

### Type Classes

    class EncodeJson a where
      encodeJson :: a -> Json


### Type Class Instances

    instance encodeJsonArray :: (EncodeJson a) => EncodeJson [a]

    instance encodeJsonJBoolean :: EncodeJson Boolean

    instance encodeJsonJNull :: EncodeJson Unit

    instance encodeJsonJNumber :: EncodeJson Number

    instance encodeJsonJString :: EncodeJson String

    instance encodeJsonJson :: EncodeJson Json

    instance encodeMap :: (EncodeJson a) => EncodeJson (M.StrMap a)


## Module Data.Argonaut.JCursor

### Types

    data JCursor where
      JCursorTop :: JCursor
      JField :: JCursor -> String -> JCursor
      JIndex :: JCursor -> Number -> JCursor

    newtype JsonPrim where
      JsonPrim :: (forall a. (JNull -> a) -> (JBoolean -> a) -> (JNumber -> a) -> (JString -> a) -> a) -> JsonPrim


### Type Class Instances

    instance eqJCursor :: Eq JCursor

    instance monoidJCursor :: Monoid JCursor

    instance ordJCursor :: Ord JCursor

    instance semigroupJCursor :: Semigroup JCursor

    instance showJCursor :: Show JCursor

    instance showJsonPrim :: Show JsonPrim


### Values

    cursorGet :: JCursor -> Json -> Maybe Json

    cursorSet :: JCursor -> Json -> Json -> Maybe Json

    downField :: String -> JCursor -> JCursor

    downIndex :: Number -> JCursor -> JCursor

    fromPrims :: [Tuple JCursor JsonPrim] -> Maybe Json

    insideOut :: JCursor -> JCursor

    toPrims :: Json -> [Tuple JCursor JsonPrim]


## Module Data.Argonaut.Parser

### Values

    jsonParser :: String -> Either String Json


## Module Data.Argonaut.Printer

### Type Classes

    class Printer a where
      printJson :: Json -> a


### Type Class Instances

    instance printerString :: Printer String



