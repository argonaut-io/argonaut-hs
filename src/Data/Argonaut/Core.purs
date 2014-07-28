module Data.Argonaut.Core where

  import Control.Lens (filtered, prism', PrismP(), TraversalP())

  import Data.Maybe (Maybe(..))

  import qualified Data.Map as M

  type JObject = M.Map JString Json
  type JArray = [Json]
  type JString = String
  type JNumber = Number
  type JBoolean = Boolean
  type JNull = Unit

  data Json = JsonNull    JNull
            | JsonBoolean JBoolean
            | JsonNumber  JNumber
            | JsonString  JString
            | JsonArray   JArray
            | JsonObject  JObject

  instance eqJson :: Eq Json where
    (==) (JsonNull    _) (JsonNull    _)  = true
    (==) (JsonBoolean b) (JsonBoolean b') = b == b'
    (==) (JsonNumber  n) (JsonNumber  n') = n == n'
    (==) (JsonString  s) (JsonString  s') = s == s'
    (==) (JsonArray   a) (JsonArray   a') = a == a'
    (==) (JsonObject  o) (JsonObject  o') = o == o'
    (==) _               _                = false

    (/=) j               j'               = not (j == j')

  -- Folds

  foldJson :: forall a
           .  (Unit     -> a)
           -> (JBoolean -> a)
           -> (JNumber  -> a)
           -> (JString  -> a)
           -> (JArray   -> a)
           -> (JObject  -> a)
           -> Json
           -> a
  foldJson jsonNull _ _ _ _ _    (JsonNull    _)   = jsonNull    unit
  foldJson _ jsonBoolean _ _ _ _ (JsonBoolean val) = jsonBoolean val
  foldJson _ _ jsonNumber _ _ _  (JsonNumber  val) = jsonNumber  val
  foldJson _ _ _ jsonString _ _  (JsonString  val) = jsonString  val
  foldJson _ _ _ _ jsonArray _   (JsonArray   val) = jsonArray   val
  foldJson _ _ _ _ _ jsonObject  (JsonObject  val) = jsonObject  val

  foldJsonNull :: forall a. a -> (JNull -> a) -> Json -> a
  foldJsonNull _   f (JsonNull _) = f unit
  foldJsonNull def _ _            = def

  foldJsonBoolean :: forall a. a -> (JBoolean -> a) -> Json -> a
  foldJsonBoolean _   f (JsonBoolean val) = f val
  foldJsonBoolean def _ _                 = def

  foldJsonNumber :: forall a. a -> (JNumber -> a) -> Json -> a
  foldJsonNumber _   f (JsonNumber val) = f val
  foldJsonNumber def _ _                = def

  foldJsonString :: forall a. a -> (JString -> a) -> Json -> a
  foldJsonString _   f (JsonString val) = f val
  foldJsonString def _ _                = def

  foldJsonArray :: forall a. a -> (JArray -> a) -> Json -> a
  foldJsonArray _   f (JsonArray val) = f val
  foldJsonArray def _ _               = def

  foldJsonObject :: forall a. a -> (JObject -> a) -> Json -> a
  foldJsonObject _   f (JsonObject val) = f val
  foldJsonObject def _ _                = def

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

  fromNull :: JNull -> Json
  fromNull = JsonNull
  fromBoolean :: JBoolean -> Json
  fromBoolean = JsonBoolean
  fromNumber :: JNumber -> Json
  fromNumber = JsonNumber
  fromString :: JString -> Json
  fromString = JsonString
  fromArray :: JArray -> Json
  fromArray = JsonArray
  fromObject :: JObject -> Json
  fromObject = JsonObject

  -- Default values

  jsonTrue :: Json
  jsonTrue = fromBoolean true
  jsonFalse :: Json
  jsonFalse = fromBoolean false
  jsonZero :: Json
  jsonZero = fromNumber 0
  jsonNull :: Json
  jsonNull = fromNull unit
  jsonEmptyString :: Json
  jsonEmptyString = fromString ""
  jsonEmptyArray :: Json
  jsonEmptyArray = fromArray []
  jsonEmptyObject :: Json
  jsonEmptyObject = fromObject M.empty
  jsonSingletonArray :: Json -> Json
  jsonSingletonArray j = fromArray [j]
  jsonSingletonObject :: JString -> Json -> Json
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
