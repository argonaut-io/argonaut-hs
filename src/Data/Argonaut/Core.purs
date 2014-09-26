module Data.Argonaut.Core where

  import Control.Lens (filtered, prism', PrismP(), TraversalP())

  import Data.Maybe (Maybe(..))
  import Data.Tuple (Tuple(..))
  import qualified Data.StrMap as M
  import Data.Function

  foreign import data JNull :: *

  instance eqJNull :: Eq JNull where
    (==) n1 n2 = true

    (/=) n1 n2 = false 

  type JBoolean = Boolean
  type JNumber  = Number
  type JString  = String
  type JField   = String
  type JAssoc   = Tuple JField Json
  type JArray   = [Json]
  type JObject  = M.StrMap Json

  foreign import data Json :: *

  foreign import _stringify "function _stringify(j){ return JSON.stringify(j); }" :: Json -> String

  instance showJson :: Show Json where
    show = _stringify

  testType :: forall a. (Eq a) => (forall b. b -> (a -> b) -> Json -> b) -> Json -> (a -> Boolean)
  testType f j = \v1 -> f false (\v2 -> v1 == v2) j

  instance eqJson :: Eq Json where
    (==) j1 j2 = foldJson a b c d e f j1 where 
      a = testType foldJsonNull j2
      b = testType foldJsonBoolean j2
      c = testType foldJsonNumber j2
      d = testType foldJsonString j2
      e = testType foldJsonArray j2
      f = testType foldJsonObject j2

    (/=) j j' = not (j == j')

  -- Folds

  foreign import _foldJson
    "function _foldJson(isNull, isBool, isNum, isStr, isArr, isObj, j) {\
    \   if (j == null) return isNull(null);                             \
    \   else if (typeof j === 'boolean') return isBool(j);              \
    \   else if (typeof j === 'number') return isNum(j);                \
    \   else if (typeof j === 'string') return isStr(j);                \
    \   else if (Object.prototype.toString.call(j) === '[object Array]') return isArr(j); \
    \   else return isObj(j);                                           \
    \}" :: forall z. Fn7 (JNull -> z) (JBoolean -> z) (JNumber -> z) (JString -> z) (JArray -> z) (JObject -> z) Json z

  foldJson :: forall a
           .  (JNull    -> a)
           -> (JBoolean -> a)
           -> (JNumber  -> a)
           -> (JString  -> a)
           -> (JArray   -> a)
           -> (JObject  -> a)
           -> Json
           -> a
  foldJson a b c d e f json = runFn7 _foldJson a b c d e f json

  foldJsonNull :: forall a. a -> (JNull -> a) -> Json -> a
  foldJsonNull d f j = runFn7 _foldJson f (const d) (const d) (const d) (const d) (const d) j 

  foldJsonBoolean :: forall a. a -> (JBoolean -> a) -> Json -> a
  foldJsonBoolean d f j = runFn7 _foldJson (const d) f (const d) (const d) (const d) (const d) j 

  foldJsonNumber :: forall a. a -> (JNumber -> a) -> Json -> a
  foldJsonNumber d f j = runFn7 _foldJson (const d) (const d) f (const d) (const d) (const d) j 

  foldJsonString :: forall a. a -> (JString -> a) -> Json -> a
  foldJsonString d f j = runFn7 _foldJson (const d) (const d) (const d) f (const d) (const d) j 

  foldJsonArray :: forall a. a -> (JArray -> a) -> Json -> a
  foldJsonArray d f j = runFn7 _foldJson (const d) (const d) (const d) (const d) f (const d) j 

  foldJsonObject :: forall a. a -> (JObject -> a) -> Json -> a
  foldJsonObject d f j = runFn7 _foldJson (const d) (const d) (const d) (const d) (const d) f j 

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

  foreign import fromNull     "function fromNull(_){return null;}"  :: JNull -> Json
  foreign import fromBoolean  "function fromBoolean(b){return b;}"  :: JBoolean -> Json
  foreign import fromNumber   "function fromNumber(n){return n;}"   :: JNumber -> Json
  foreign import fromString   "function fromString(s){return s;}"   :: JString -> Json
  foreign import fromArray    "function fromArray(a){return a;}"    :: JArray -> Json
  foreign import fromObject   "function fromObject(o){return o;}"   :: JObject -> Json
  
  -- Default values

  jsonTrue :: Json
  jsonTrue = fromBoolean true
  jsonFalse :: Json
  jsonFalse = fromBoolean false
  jsonZero :: Json
  jsonZero = fromNumber 0
  foreign import jsonNull "var jsonNull = null;" :: Json 
  jsonEmptyString :: Json
  jsonEmptyString = fromString ""
  jsonEmptyArray :: Json
  jsonEmptyArray = fromArray []
  jsonEmptyObject :: Json
  jsonEmptyObject = fromObject M.empty
  jsonSingletonArray :: Json -> Json
  jsonSingletonArray j = fromArray [j]
  jsonSingletonObject :: JField -> Json -> Json
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
