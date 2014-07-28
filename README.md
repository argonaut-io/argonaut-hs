# Module Documentation

## Module Data.Argonaut

## Module Data.Argonaut.Core

### Types

    type JArray  = [Json]

    type JBoolean  = Boolean

    type JNull  = Unit

    type JNumber  = Number

    type JObject  = M.Map JString Json

    type JString  = String

    data Json where
      JsonNull :: JNull -> Json
      JsonBoolean :: JBoolean -> Json
      JsonNumber :: JNumber -> Json
      JsonString :: JString -> Json
      JsonArray :: JArray -> Json
      JsonObject :: JObject -> Json


### Type Class Instances

    instance eqJson :: Eq Json


### Values

    arrayL :: PrismP Json JArray

    booleanL :: PrismP Json JBoolean

    foldJson :: forall a. (Unit -> a) -> (JBoolean -> a) -> (JNumber -> a) -> (JString -> a) -> (JArray -> a) -> (JObject -> a) -> Json -> a

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

    jsonSingletonObject :: JString -> Json -> Json

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

    class DecodeJson m n a where
      decodeJson :: m Json -> n a


### Type Class Instances

    instance decodeJsonIdESDRArray :: DecodeJson Identity (Either String) [Json]

    instance decodeJsonIdESDRBoolean :: DecodeJson Identity (Either String) Boolean

    instance decodeJsonIdESDRJson :: DecodeJson Identity (Either String) Json

    instance decodeJsonIdESDRNull :: DecodeJson Identity (Either String) Unit

    instance decodeJsonIdESDRNumber :: DecodeJson Identity (Either String) Number

    instance decodeJsonIdESDRObject :: DecodeJson Identity (Either String) (M.Map String Json)

    instance decodeJsonIdESDRString :: DecodeJson Identity (Either String) String


### Values

    arrayIndexL :: forall a. (DecodeJson Identity (Either String) a, EncodeJson Identity Identity a) => JNumber -> TraversalP Json a

    decodeFrom :: forall m a n. (DecodeJson m n a) => m Json -> n a

    decodeL :: forall a. (DecodeJson Identity (Either String) a, EncodeJson Identity Identity a) => PrismP Json a

    decodeMaybe :: forall a. (DecodeJson Identity (Either String) a) => Json -> Maybe a

    objectFieldL :: forall a. (DecodeJson Identity (Either String) a, EncodeJson Identity Identity a) => JString -> TraversalP Json a


## Module Data.Argonaut.Encode

### Type Classes

    class EncodeJson m n a where
      encodeJson :: m a -> n Json


### Type Class Instances

    instance encodeJsonIdIdJArray :: EncodeJson Identity Identity [Json]

    instance encodeJsonIdIdJBoolean :: EncodeJson Identity Identity Boolean

    instance encodeJsonIdIdJNull :: EncodeJson Identity Identity Unit

    instance encodeJsonIdIdJNumber :: EncodeJson Identity Identity Number

    instance encodeJsonIdIdJObject :: EncodeJson Identity Identity (M.Map String Json)

    instance encodeJsonIdIdJString :: EncodeJson Identity Identity String

    instance encodeJsonIdIdJson :: EncodeJson Identity Identity Json


### Values

    encodeIdentity :: forall a. (EncodeJson Identity Identity a) => a -> Json

    encodeTo :: forall m a n. (EncodeJson m n a) => m a -> n Json


## Module Data.Argonaut.Parser

## Module Data.Argonaut.Printer


