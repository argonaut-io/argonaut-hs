# Module Documentation

## Module Data.Argonaut.Combinators

#### `(:=)`

``` purescript
(:=) :: forall a. (EncodeJson a) => String -> a -> JAssoc
```


#### `(~>)`

``` purescript
(~>) :: forall a. (EncodeJson a) => JAssoc -> a -> Json
```


#### `(?>>=)`

``` purescript
(?>>=) :: forall a b. Maybe a -> String -> Either String a
```


#### `(.?)`

``` purescript
(.?) :: forall a. (DecodeJson a) => JObject -> String -> Either String a
```


## Module Data.Argonaut.Core

#### `JBoolean`

``` purescript
type JBoolean = Boolean
```


#### `JNumber`

``` purescript
type JNumber = Number
```


#### `JString`

``` purescript
type JString = String
```


#### `JAssoc`

``` purescript
type JAssoc = Tuple String Json
```


#### `JArray`

``` purescript
type JArray = [Json]
```


#### `JObject`

``` purescript
type JObject = M.StrMap Json
```


#### `JNull`

``` purescript
data JNull :: *
```


#### `Json`

``` purescript
data Json :: *
```


#### `foldJson`

``` purescript
foldJson :: forall a. (JNull -> a) -> (JBoolean -> a) -> (JNumber -> a) -> (JString -> a) -> (JArray -> a) -> (JObject -> a) -> Json -> a
```

#### `foldJsonNull`

``` purescript
foldJsonNull :: forall a. a -> (JNull -> a) -> Json -> a
```


#### `foldJsonBoolean`

``` purescript
foldJsonBoolean :: forall a. a -> (JBoolean -> a) -> Json -> a
```


#### `foldJsonNumber`

``` purescript
foldJsonNumber :: forall a. a -> (JNumber -> a) -> Json -> a
```


#### `foldJsonString`

``` purescript
foldJsonString :: forall a. a -> (JString -> a) -> Json -> a
```


#### `foldJsonArray`

``` purescript
foldJsonArray :: forall a. a -> (JArray -> a) -> Json -> a
```


#### `foldJsonObject`

``` purescript
foldJsonObject :: forall a. a -> (JObject -> a) -> Json -> a
```


#### `isJsonType`

``` purescript
isJsonType :: forall a. (Boolean -> (a -> Boolean) -> Json -> Boolean) -> Json -> Boolean
```

#### `isNull`

``` purescript
isNull :: Json -> Boolean
```


#### `isBoolean`

``` purescript
isBoolean :: Json -> Boolean
```


#### `isNumber`

``` purescript
isNumber :: Json -> Boolean
```


#### `isString`

``` purescript
isString :: Json -> Boolean
```


#### `isArray`

``` purescript
isArray :: Json -> Boolean
```


#### `isObject`

``` purescript
isObject :: Json -> Boolean
```


#### `toNull`

``` purescript
toNull :: Json -> Maybe JNull
```


#### `toBoolean`

``` purescript
toBoolean :: Json -> Maybe JBoolean
```


#### `toNumber`

``` purescript
toNumber :: Json -> Maybe JNumber
```


#### `toString`

``` purescript
toString :: Json -> Maybe JString
```


#### `toArray`

``` purescript
toArray :: Json -> Maybe JArray
```


#### `toObject`

``` purescript
toObject :: Json -> Maybe JObject
```


#### `fromNull`

``` purescript
fromNull :: JNull -> Json
```

#### `fromBoolean`

``` purescript
fromBoolean :: JBoolean -> Json
```


#### `fromNumber`

``` purescript
fromNumber :: JNumber -> Json
```


#### `fromString`

``` purescript
fromString :: JString -> Json
```


#### `fromArray`

``` purescript
fromArray :: JArray -> Json
```


#### `fromObject`

``` purescript
fromObject :: JObject -> Json
```


#### `jsonTrue`

``` purescript
jsonTrue :: Json
```

#### `jsonFalse`

``` purescript
jsonFalse :: Json
```


#### `jsonZero`

``` purescript
jsonZero :: Json
```


#### `jsonNull`

``` purescript
jsonNull :: Json
```


#### `jsonEmptyString`

``` purescript
jsonEmptyString :: Json
```


#### `jsonEmptyArray`

``` purescript
jsonEmptyArray :: Json
```


#### `jsonEmptyObject`

``` purescript
jsonEmptyObject :: Json
```


#### `jsonSingletonArray`

``` purescript
jsonSingletonArray :: Json -> Json
```


#### `jsonSingletonObject`

``` purescript
jsonSingletonObject :: String -> Json -> Json
```


#### `nullL`

``` purescript
nullL :: PrismP Json JNull
```

#### `booleanL`

``` purescript
booleanL :: PrismP Json JBoolean
```


#### `numberL`

``` purescript
numberL :: PrismP Json JNumber
```


#### `stringL`

``` purescript
stringL :: PrismP Json JString
```


#### `arrayL`

``` purescript
arrayL :: PrismP Json JArray
```


#### `objectL`

``` purescript
objectL :: PrismP Json JObject
```


#### `jsonNullL`

``` purescript
jsonNullL :: TraversalP Json Json
```

#### `jsonBooleanL`

``` purescript
jsonBooleanL :: TraversalP Json Json
```


#### `jsonNumberL`

``` purescript
jsonNumberL :: TraversalP Json Json
```


#### `jsonStringL`

``` purescript
jsonStringL :: TraversalP Json Json
```


#### `jsonArrayL`

``` purescript
jsonArrayL :: TraversalP Json Json
```


#### `jsonObjectL`

``` purescript
jsonObjectL :: TraversalP Json Json
```


#### `eqJNull`

``` purescript
instance eqJNull :: Eq JNull
```


#### `ordJNull`

``` purescript
instance ordJNull :: Ord JNull
```


#### `showJson`

``` purescript
instance showJson :: Show Json
```


#### `showJsonNull`

``` purescript
instance showJsonNull :: Show JNull
```


#### `eqJson`

``` purescript
instance eqJson :: Eq Json
```


#### `ordJson`

``` purescript
instance ordJson :: Ord Json
```



## Module Data.Argonaut.Decode

#### `DecodeJson`

``` purescript
class DecodeJson a where
  decodeJson :: Json -> Either String a
```


#### `decodeJsonMaybe`

``` purescript
instance decodeJsonMaybe :: (DecodeJson a) => DecodeJson (Maybe a)
```


#### `decodeJsonTuple`

``` purescript
instance decodeJsonTuple :: (DecodeJson a, DecodeJson b) => DecodeJson (Tuple a b)
```


#### `decodeJsonEither`

``` purescript
instance decodeJsonEither :: (DecodeJson a, DecodeJson b) => DecodeJson (Either a b)
```


#### `decodeJsonNull`

``` purescript
instance decodeJsonNull :: DecodeJson Unit
```


#### `decodeJsonBoolean`

``` purescript
instance decodeJsonBoolean :: DecodeJson Boolean
```


#### `decodeJsonNumber`

``` purescript
instance decodeJsonNumber :: DecodeJson Number
```


#### `decodeJsonString`

``` purescript
instance decodeJsonString :: DecodeJson String
```


#### `decodeJsonJson`

``` purescript
instance decodeJsonJson :: DecodeJson Json
```


#### `decodeJsonChar`

``` purescript
instance decodeJsonChar :: DecodeJson Char
```


#### `decodeStrMap`

``` purescript
instance decodeStrMap :: (DecodeJson a) => DecodeJson (M.StrMap a)
```


#### `decodeArray`

``` purescript
instance decodeArray :: (DecodeJson a) => DecodeJson [a]
```


#### `decodeMap`

``` purescript
instance decodeMap :: (Ord a, DecodeJson a, DecodeJson b) => DecodeJson (Map.Map a b)
```


#### `decodeMaybe`

``` purescript
decodeMaybe :: forall a. (DecodeJson a) => Json -> Maybe a
```


#### `decodeL`

``` purescript
decodeL :: forall a. (DecodeJson a, EncodeJson a) => PrismP Json a
```


#### `arrayIndexL`

``` purescript
arrayIndexL :: forall a. (DecodeJson a, EncodeJson a) => JNumber -> TraversalP Json a
```


#### `objectFieldL`

``` purescript
objectFieldL :: forall a. (DecodeJson a, EncodeJson a) => JString -> TraversalP Json a
```



## Module Data.Argonaut.Encode

#### `EncodeJson`

``` purescript
class EncodeJson a where
  encodeJson :: a -> Json
```


#### `encodeJsonMaybe`

``` purescript
instance encodeJsonMaybe :: (EncodeJson a) => EncodeJson (Maybe a)
```


#### `encodeJsonTuple`

``` purescript
instance encodeJsonTuple :: (EncodeJson a, EncodeJson b) => EncodeJson (Tuple a b)
```


#### `encodeJsonEither`

``` purescript
instance encodeJsonEither :: (EncodeJson a, EncodeJson b) => EncodeJson (Either a b)
```


#### `encodeJsonUnit`

``` purescript
instance encodeJsonUnit :: EncodeJson Unit
```


#### `encodeJsonJBoolean`

``` purescript
instance encodeJsonJBoolean :: EncodeJson Boolean
```


#### `encodeJsonJNumber`

``` purescript
instance encodeJsonJNumber :: EncodeJson Number
```


#### `encodeJsonJString`

``` purescript
instance encodeJsonJString :: EncodeJson String
```


#### `encodeJsonJson`

``` purescript
instance encodeJsonJson :: EncodeJson Json
```


#### `encodeJsonChar`

``` purescript
instance encodeJsonChar :: EncodeJson Char
```


#### `encodeJsonArray`

``` purescript
instance encodeJsonArray :: (EncodeJson a) => EncodeJson [a]
```


#### `encodeStrMap`

``` purescript
instance encodeStrMap :: (EncodeJson a) => EncodeJson (M.StrMap a)
```


#### `encodeMap`

``` purescript
instance encodeMap :: (Ord a, EncodeJson a, EncodeJson b) => EncodeJson (Map.Map a b)
```



## Module Data.Argonaut.JCursor

#### `JCursor`

``` purescript
data JCursor
  = JCursorTop 
  | JField String JCursor
  | JIndex Number JCursor
```


#### `JsonPrim`

``` purescript
newtype JsonPrim
  = JsonPrim (forall a. (JNull -> a) -> (JBoolean -> a) -> (JNumber -> a) -> (JString -> a) -> a)
```


#### `runJsonPrim`

``` purescript
runJsonPrim :: JsonPrim -> (forall a. (JNull -> a) -> (JBoolean -> a) -> (JNumber -> a) -> (JString -> a) -> a)
```


#### `primNull`

``` purescript
primNull :: JsonPrim
```


#### `primBool`

``` purescript
primBool :: JBoolean -> JsonPrim
```


#### `primNum`

``` purescript
primNum :: JNumber -> JsonPrim
```


#### `primStr`

``` purescript
primStr :: JString -> JsonPrim
```


#### `primToJson`

``` purescript
primToJson :: JsonPrim -> Json
```


#### `insideOut`

``` purescript
insideOut :: JCursor -> JCursor
```


#### `downField`

``` purescript
downField :: String -> JCursor -> JCursor
```


#### `downIndex`

``` purescript
downIndex :: Number -> JCursor -> JCursor
```


#### `cursorGet`

``` purescript
cursorGet :: JCursor -> Json -> Maybe Json
```


#### `cursorSet`

``` purescript
cursorSet :: JCursor -> Json -> Json -> Maybe Json
```


#### `toPrims`

``` purescript
toPrims :: Json -> [Tuple JCursor JsonPrim]
```


#### `fromPrims`

``` purescript
fromPrims :: [Tuple JCursor JsonPrim] -> Maybe Json
```


#### `showJCursor`

``` purescript
instance showJCursor :: Show JCursor
```


#### `showJsonPrim`

``` purescript
instance showJsonPrim :: Show JsonPrim
```


#### `eqJCursor`

``` purescript
instance eqJCursor :: Eq JCursor
```


#### `ordJCursor`

``` purescript
instance ordJCursor :: Ord JCursor
```


#### `semigroupJCursor`

``` purescript
instance semigroupJCursor :: Semigroup JCursor
```


#### `monoidJCursor`

``` purescript
instance monoidJCursor :: Monoid JCursor
```


#### `encodeJsonJCursor`

``` purescript
instance encodeJsonJCursor :: EncodeJson JCursor
```


#### `decodeJsonJCursor`

``` purescript
instance decodeJsonJCursor :: DecodeJson JCursor
```



## Module Data.Argonaut.Parser

#### `jsonParser`

``` purescript
jsonParser :: String -> Either String Json
```



## Module Data.Argonaut.Printer

#### `Printer`

``` purescript
class Printer a where
  printJson :: Json -> a
```


#### `printerString`

``` purescript
instance printerString :: Printer String
```




