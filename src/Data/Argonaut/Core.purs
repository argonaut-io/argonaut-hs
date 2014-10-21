module Data.Argonaut.Core 
  ( Json(..)
  , JNull(..)
  , JBoolean(..)
  , JNumber(..)
  , JString(..)
  , JAssoc(..)
  , JArray(..)
  , JObject(..)
  , foldJson
  , arrayL
  , booleanL
  , foldJsonArray
  , foldJsonBoolean
  , foldJsonNull
  , foldJsonNumber
  , foldJsonObject
  , foldJsonString
  , fromArray
  , fromBoolean
  , fromNull
  , fromNumber
  , fromObject
  , fromString
  , isArray
  , isBoolean
  , isJsonType
  , isNull
  , isNumber
  , isObject
  , isString
  , jsonArrayL
  , jsonBooleanL
  , jsonEmptyArray
  , jsonEmptyObject
  , jsonEmptyString
  , jsonFalse
  , jsonNull
  , jsonNullL
  , jsonNumberL
  , jsonObjectL
  , jsonSingletonArray
  , jsonSingletonObject
  , jsonStringL
  , jsonTrue
  , jsonZero
  , nullL
  , numberL
  , objectL
  , stringL
  , toArray
  , toBoolean
  , toNull
  , toNumber
  , toObject
  , toString
  ) where

  import Control.Lens (filtered, prism', PrismP(), TraversalP())

  import Data.Maybe (Maybe(..))
  import Data.Tuple (Tuple(..))
  import qualified Data.StrMap as M
  import Data.Function

  type JBoolean = Boolean
  type JNumber  = Number
  type JString  = String
  type JAssoc   = Tuple String Json
  type JArray   = [Json]
  type JObject  = M.StrMap Json

  foreign import data JNull :: *
  foreign import data Json :: *

  -- Folds

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
  jsonSingletonObject :: String -> Json -> Json
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

  instance eqJNull :: Eq JNull where
    (==) n1 n2 = true

    (/=) n1 n2 = false 

  instance ordJNull :: Ord JNull where 
    compare = const <<< const EQ

  instance showJson :: Show Json where
    show = _stringify

  instance showJsonNull :: Show JNull where 
    show = const "null"

  instance eqJson :: Eq Json where
    (==) j1 j2 = _stringify j1 == _stringify j2

    (/=) j j' = not (j == j')

  instance ordJson :: Ord Json where
    compare a b = runFn5 _compare EQ GT LT a b    

  foreign import _stringify "function _stringify(j){ return JSON.stringify(j); }" :: Json -> String

  foreign import _foldJson
    "function _foldJson(isNull, isBool, isNum, isStr, isArr, isObj, j) {\
    \   if (j == null) return isNull(null);                             \
    \   else if (typeof j === 'boolean') return isBool(j);              \
    \   else if (typeof j === 'number') return isNum(j);                \
    \   else if (typeof j === 'string') return isStr(j);                \
    \   else if (Object.prototype.toString.call(j) === '[object Array]') return isArr(j); \
    \   else return isObj(j);                                           \
    \}" :: forall z. Fn7 (JNull -> z) (JBoolean -> z) (JNumber -> z) (JString -> z) (JArray -> z) (JObject -> z) Json z

  -- very fast ordering for Json
  foreign import _compare
    """
    function _compare(EQ, GT, LT, a, b) {
      function isArray(a) {
        return Object.prototype.toString.call(a) === '[object Array]';
      }
      function keys(o) {
        var a = [];
        for (var k in o) {
          a.push(k);
        }
        return a;
      }

      if (a == null) {
        if (b == null) return EQ;
        else return LT;
      } else if (typeof a === 'boolean') {
        if (typeof b === 'boolean') {
          // boolean / boolean 
          if (a === b) return EQ;
          else if (a == false) return LT;
          else return GT;
        } else if (b == null) return GT;
        else return LT;
      } else if (typeof a === 'number') {
        if (typeof b === 'number') {
          if (a === b) return EQ;
          else if (a < b) return LT;
          else return GT;
        } else if (b == null) return GT;
        else if (typeof b === 'boolean') return GT;
        else return LT;
      } else if (typeof a === 'string') {
        if (typeof b === 'string') {
          if (a === b) return EQ;
          else if (a < b) return LT;
          else return GT;
        } else if (b == null) return GT;
        else if (typeof b === 'boolean') return GT;
        else if (typeof b === 'number') return GT;
        else return LT;
      } else if (isArray(a)) {
        if (isArray(b)) {
          for (var i = 0; i < Math.min(a.length, b.length); i++) {
            var c = _compare(EQ, GT, LT, a[i], b[i]);

            if (c !== EQ) return c;
          }
          if (a.length === b.length) return EQ;
          else if (a.length < b.length) return LT;
          else return GT;
        } else if (b == null) return GT;
        else if (typeof b === 'boolean') return GT;
        else if (typeof b === 'number') return GT;
        else if (typeof b === 'string') return GT;
        else return LT;
      }
      else {
        if (b == null) return GT;
        else if (typeof b === 'boolean') return GT;
        else if (typeof b === 'number') return GT;
        else if (typeof b === 'string') return GT;
        else if (isArray(b)) return GT;
        else {
          var akeys = keys(a);
          var bkeys = keys(b);

          var keys = akeys.concat(bkeys).sort();

          for (var i = 0; i < keys.length; i++) {
            var k = keys[i];

            if (a[k] === undefined) return LT;
            else if (b[k] === undefined) return GT;

            var c = _compare(EQ, GT, LT, a[k], b[k]);

            if (c !== EQ) return c;
          }

          if (akeys.length === bkeys.length) return EQ;
          else if (akeys.length < bkeys.length) return LT;
          else return GT;
        }
      }
    }
    """ :: Fn5 Ordering Ordering Ordering Json Json Ordering