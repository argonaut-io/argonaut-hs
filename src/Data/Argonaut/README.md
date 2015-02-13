# Module Documentation

## Module Data.Argonaut.Combinators

### Values

#### `(.?)`

     obj .? "foo"

    (.?) :: forall a. (DecodeJson a) => JObject -> String -> Either String a

#### `(:=)`

    (:=) :: forall a. (EncodeJson a) => String -> a -> JAssoc

#### `(?>>=)`

    (?>>=) :: forall a b. Maybe a -> String -> Either String a

#### `(~>)`

    (~>) :: forall a. (EncodeJson a) => JAssoc -> a -> Json


## Module Data.Argonaut.Core

### Types

#### `JArray`

    type JArray = [Json]

#### `JAssoc`

    type JAssoc = Tuple String Json

#### `JBoolean`

    type JBoolean = Boolean

#### `JNull`

    data JNull :: *

#### `JNumber`

    type JNumber = Number

#### `JObject`

    type JObject = M.StrMap Json

#### `JString`

    type JString = String

#### `Json`

    data Json :: *


### Type Class Instances

#### `eqJNull`

    instance eqJNull :: Eq JNull

#### `eqJson`

    instance eqJson :: Eq Json

#### `ordJNull`

    instance ordJNull :: Ord JNull

#### `ordJson`

    instance ordJson :: Ord Json

#### `showJson`

    instance showJson :: Show Json

#### `showJsonNull`

    instance showJsonNull :: Show JNull


### Values

#### `arrayL`

    arrayL :: PrismP Json JArray

#### `booleanL`

    booleanL :: PrismP Json JBoolean

#### `foldJson`

     Folds

    foldJson :: forall a. (JNull -> a) -> (JBoolean -> a) -> (JNumber -> a) -> (JString -> a) -> (JArray -> a) -> (JObject -> a) -> Json -> a

#### `foldJsonArray`

    foldJsonArray :: forall a. a -> (JArray -> a) -> Json -> a

#### `foldJsonBoolean`

    foldJsonBoolean :: forall a. a -> (JBoolean -> a) -> Json -> a

#### `foldJsonNull`

    foldJsonNull :: forall a. a -> (JNull -> a) -> Json -> a

#### `foldJsonNumber`

    foldJsonNumber :: forall a. a -> (JNumber -> a) -> Json -> a

#### `foldJsonObject`

    foldJsonObject :: forall a. a -> (JObject -> a) -> Json -> a

#### `foldJsonString`

    foldJsonString :: forall a. a -> (JString -> a) -> Json -> a

#### `fromArray`

    fromArray :: JArray -> Json

#### `fromBoolean`

    fromBoolean :: JBoolean -> Json

#### `fromNull`

     Encoding

    fromNull :: JNull -> Json

#### `fromNumber`

    fromNumber :: JNumber -> Json

#### `fromObject`

    fromObject :: JObject -> Json

#### `fromString`

    fromString :: JString -> Json

#### `isArray`

    isArray :: Json -> Boolean

#### `isBoolean`

    isBoolean :: Json -> Boolean

#### `isJsonType`

     Tests

    isJsonType :: forall a. (Boolean -> (a -> Boolean) -> Json -> Boolean) -> Json -> Boolean

#### `isNull`

    isNull :: Json -> Boolean

#### `isNumber`

    isNumber :: Json -> Boolean

#### `isObject`

    isObject :: Json -> Boolean

#### `isString`

    isString :: Json -> Boolean

#### `jsonArrayL`

    jsonArrayL :: TraversalP Json Json

#### `jsonBooleanL`

    jsonBooleanL :: TraversalP Json Json

#### `jsonEmptyArray`

    jsonEmptyArray :: Json

#### `jsonEmptyObject`

    jsonEmptyObject :: Json

#### `jsonEmptyString`

    jsonEmptyString :: Json

#### `jsonFalse`

    jsonFalse :: Json

#### `jsonNull`

    jsonNull :: Json

#### `jsonNullL`

     Traversals

    jsonNullL :: TraversalP Json Json

#### `jsonNumberL`

    jsonNumberL :: TraversalP Json Json

#### `jsonObjectL`

    jsonObjectL :: TraversalP Json Json

#### `jsonSingletonArray`

    jsonSingletonArray :: Json -> Json

#### `jsonSingletonObject`

    jsonSingletonObject :: String -> Json -> Json

#### `jsonStringL`

    jsonStringL :: TraversalP Json Json

#### `jsonTrue`

     Default values

    jsonTrue :: Json

#### `jsonZero`

    jsonZero :: Json

#### `nullL`

     Prisms

    nullL :: PrismP Json JNull

#### `numberL`

    numberL :: PrismP Json JNumber

#### `objectL`

    objectL :: PrismP Json JObject

#### `stringL`

    stringL :: PrismP Json JString

#### `toArray`

    toArray :: Json -> Maybe JArray

#### `toBoolean`

    toBoolean :: Json -> Maybe JBoolean

#### `toNull`

    toNull :: Json -> Maybe JNull

#### `toNumber`

    toNumber :: Json -> Maybe JNumber

#### `toObject`

    toObject :: Json -> Maybe JObject

#### `toString`

    toString :: Json -> Maybe JString


## Module Data.Argonaut.Decode

### Type Classes

#### `DecodeJson`

    class DecodeJson a where
      decodeJson :: Json -> Either String a


### Type Class Instances

#### `decodeArray`

    instance decodeArray :: (DecodeJson a) => DecodeJson [a]

#### `decodeJsonBoolean`

    instance decodeJsonBoolean :: DecodeJson Boolean

#### `decodeJsonChar`

    instance decodeJsonChar :: DecodeJson Char

#### `decodeJsonEither`

    instance decodeJsonEither :: (DecodeJson a, DecodeJson b) => DecodeJson (Either a b)

#### `decodeJsonJson`

    instance decodeJsonJson :: DecodeJson Json

#### `decodeJsonMaybe`

    instance decodeJsonMaybe :: (DecodeJson a) => DecodeJson (Maybe a)

#### `decodeJsonNull`

    instance decodeJsonNull :: DecodeJson Unit

#### `decodeJsonNumber`

    instance decodeJsonNumber :: DecodeJson Number

#### `decodeJsonString`

    instance decodeJsonString :: DecodeJson String

#### `decodeJsonTuple`

    instance decodeJsonTuple :: (DecodeJson a, DecodeJson b) => DecodeJson (Tuple a b)

#### `decodeMap`

    instance decodeMap :: (Ord a, DecodeJson a, DecodeJson b) => DecodeJson (Map.Map a b)

#### `decodeStrMap`

    instance decodeStrMap :: (DecodeJson a) => DecodeJson (M.StrMap a)


### Values

#### `arrayIndexL`

    arrayIndexL :: forall a. (DecodeJson a, EncodeJson a) => JNumber -> TraversalP Json a

#### `decodeL`

    decodeL :: forall a. (DecodeJson a, EncodeJson a) => PrismP Json a

#### `decodeMaybe`

    decodeMaybe :: forall a. (DecodeJson a) => Json -> Maybe a

#### `objectFieldL`

    objectFieldL :: forall a. (DecodeJson a, EncodeJson a) => JString -> TraversalP Json a


## Module Data.Argonaut.Encode

### Type Classes

#### `EncodeJson`

    class EncodeJson a where
      encodeJson :: a -> Json


### Type Class Instances

#### `encodeJsonArray`

    instance encodeJsonArray :: (EncodeJson a) => EncodeJson [a]

#### `encodeJsonChar`

    instance encodeJsonChar :: EncodeJson Char

#### `encodeJsonEither`

    instance encodeJsonEither :: (EncodeJson a, EncodeJson b) => EncodeJson (Either a b)

#### `encodeJsonJBoolean`

    instance encodeJsonJBoolean :: EncodeJson Boolean

#### `encodeJsonJNumber`

    instance encodeJsonJNumber :: EncodeJson Number

#### `encodeJsonJString`

    instance encodeJsonJString :: EncodeJson String

#### `encodeJsonJson`

    instance encodeJsonJson :: EncodeJson Json

#### `encodeJsonMaybe`

    instance encodeJsonMaybe :: (EncodeJson a) => EncodeJson (Maybe a)

#### `encodeJsonTuple`

    instance encodeJsonTuple :: (EncodeJson a, EncodeJson b) => EncodeJson (Tuple a b)

#### `encodeJsonUnit`

    instance encodeJsonUnit :: EncodeJson Unit

#### `encodeMap`

    instance encodeMap :: (Ord a, EncodeJson a, EncodeJson b) => EncodeJson (Map.Map a b)

#### `encodeStrMap`

    instance encodeStrMap :: (EncodeJson a) => EncodeJson (M.StrMap a)


## Module Data.Argonaut.JCursor

### Types

#### `JCursor`

    data JCursor
      = JCursorTop 
      | JField String JCursor
      | JIndex Number JCursor

#### `JsonPrim`

    newtype JsonPrim
      = JsonPrim (forall a. (JNull -> a) -> (JBoolean -> a) -> (JNumber -> a) -> (JString -> a) -> a)


### Type Class Instances

#### `decodeJsonJCursor`

    instance decodeJsonJCursor :: DecodeJson JCursor

#### `encodeJsonJCursor`

    instance encodeJsonJCursor :: EncodeJson JCursor

#### `eqJCursor`

    instance eqJCursor :: Eq JCursor

#### `monoidJCursor`

    instance monoidJCursor :: Monoid JCursor

#### `ordJCursor`

    instance ordJCursor :: Ord JCursor

#### `semigroupJCursor`

    instance semigroupJCursor :: Semigroup JCursor

#### `showJCursor`

    instance showJCursor :: Show JCursor

#### `showJsonPrim`

    instance showJsonPrim :: Show JsonPrim


### Values

#### `cursorGet`

    cursorGet :: JCursor -> Json -> Maybe Json

#### `cursorSet`

    cursorSet :: JCursor -> Json -> Json -> Maybe Json

#### `downField`

    downField :: String -> JCursor -> JCursor

#### `downIndex`

    downIndex :: Number -> JCursor -> JCursor

#### `fromPrims`

    fromPrims :: [Tuple JCursor JsonPrim] -> Maybe Json

#### `insideOut`

    insideOut :: JCursor -> JCursor

#### `primBool`

    primBool :: JBoolean -> JsonPrim

#### `primNull`

    primNull :: JsonPrim

#### `primNum`

    primNum :: JNumber -> JsonPrim

#### `primStr`

    primStr :: JString -> JsonPrim

#### `primToJson`

    primToJson :: JsonPrim -> Json

#### `runJsonPrim`

    runJsonPrim :: JsonPrim -> (forall a. (JNull -> a) -> (JBoolean -> a) -> (JNumber -> a) -> (JString -> a) -> a)

#### `toPrims`

    toPrims :: Json -> [Tuple JCursor JsonPrim]


## Module Data.Argonaut.Parser

### Values

#### `jsonParser`

    jsonParser :: String -> Either String Json


## Module Data.Argonaut.Printer

### Type Classes

#### `Printer`

    class Printer a where
      printJson :: Json -> a


### Type Class Instances

#### `printerString`

    instance printerString :: Printer String



