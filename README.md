# Module Documentation

## Module Data.Argonaut

## Module Data.Argonaut.Combinators

### Values

    (:=) :: forall a. (EncodeJson Identity Identity a) => String -> a -> JAssoc

    (~>) :: JAssoc -> Json -> Json


## Module Data.Argonaut.Core

### Types

    type JArray  = [Json]

    type JAssoc  = Tuple JField Json

    type JBoolean  = Boolean

    type JField  = String

    type JNull  = Unit

    type JNumber  = Number

    type JObject  = M.Map JField Json

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

### Types

    data ParseResult a where
      ParseFailure :: String -> ParseResult a
      ParseSuccess :: a -> ParseResult a

    type ParserState s a = { consumed :: Boolean, result :: Either ParseError a, input :: s }


### Type Classes

    class Parser m n a where
      parseJson :: m a -> n Json


### Type Class Instances

    instance applicativeParseResult :: Applicative ParseResult

    instance applyParseResult :: Apply ParseResult

    instance bindParseResult :: Bind ParseResult

    instance eqParseResult :: (Eq a) => Eq (ParseResult a)

    instance functorParseResult :: Functor ParseResult

    instance monadParseResult :: Monad ParseResult

    instance parserIdParseResultString :: Parser Identity ParseResult String

    instance showParseError :: Show ParseError

    instance showParseResult :: (Show a) => Show (ParseResult a)


### Values

    arrayParser :: Unit -> Parser String Json

    between :: forall s a m open close. (Monad m) => ParserT s m open -> ParserT s m close -> ParserT s m a -> ParserT s m a

    booleanParser :: Parser String Json

    braces :: forall s m a. (Monad m) => ParserT String m a -> ParserT String m a

    brackets :: forall m a. (Monad m) => ParserT String m a -> ParserT String m a

    digit :: Parser String String

    digits :: Parser String String

    emptyArrayParser :: Parser String Json

    emptyObjectParser :: Parser String Json

    expParser :: Parser String String

    fracParser :: Parser String String

    fromEither :: forall a. Either String a -> ParseResult a

    invalidJson :: forall a. String -> Parser String a

    isDigit :: String -> Boolean

    isoParseEither :: forall a. IsoP (ParseResult a) (Either String a)

    jsonParser :: Parser String Json

    lookAhead :: forall s a m. (Monad m) => ParserT s m a -> ParserT s m a

    manyTill :: forall s a m e. (Monad m) => ParserT s m a -> ParserT s m e -> ParserT s m [a]

    memberParser :: Unit -> Parser String JAssoc

    membersParser :: Unit -> Parser String Json

    nonEmptyArrayParser :: Parser String Json

    nonEmptyObjectParser :: Unit -> Parser String Json

    noneOf :: forall s m a. (Monad m) => [String] -> ParserT String m String

    nullParser :: Parser String Json

    numberParser :: Parser String Json

    objectParser :: Unit -> Parser String Json

    oneToNine :: String -> Boolean

    option :: forall s a m. (Monad m) => a -> ParserT s m a -> ParserT s m a

    ord :: String -> Number

    parseFrom :: forall m a n. (Parser m n a) => m a -> n Json

    parseMaybe :: forall a. (Parser Identity ParseResult a) => a -> Maybe Json

    parseString :: String -> ParseResult Json

    quoted :: forall m a. (Monad m) => ParserT String m a -> ParserT String m a

    rawStringParser :: Parser String String

    skipMany :: forall s a m. (Monad m) => ParserT s m a -> ParserT s m {  }

    skipMany1 :: forall s a m. (Monad m) => ParserT s m a -> ParserT s m {  }

    skipSpaces :: Parser String {  }

    stringParser :: Parser String Json

    toEither :: forall a. ParseResult a -> Either String a

    undefined :: forall a. a

    valueParser :: Unit -> Parser String Json


## Module Data.Argonaut.Printer

### Type Classes

    class Printer m n a where
      printJson :: m Json -> n a


### Type Class Instances

    instance printerIdIdJNull :: Printer Identity Identity String

    instance showJson :: Show Json


### Values

    printIdentity :: forall a. (Printer Identity Identity a) => Json -> a

    printTo :: forall m a n. (Printer m n a) => m Json -> n a

    printToString :: Json -> String

    stringify :: Json -> String

    stringifyArray :: JArray -> String

    stringifyBoolean :: JBoolean -> String

    stringifyField :: JField -> String

    stringifyNull :: JNull -> String

    stringifyNumber :: JNumber -> String

    stringifyObject :: JObject -> String

    stringifyString :: JString -> String



