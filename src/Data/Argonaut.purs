module Data.Argonaut
  -- Combinators
  ( (:=)
  , (~>)
  , (?>>=)
  -- Core
  -- Folds
  , foldJson
  , foldJsonNull
  , foldJsonBoolean
  , foldJsonNumber
  , foldJsonString
  , foldJsonArray
  , foldJsonObject
  -- Tests
  , isNull
  , isBoolean
  , isNumber
  , isString
  , isArray
  , isObject
  -- Decoding
  , toNull
  , toBoolean
  , toNumber
  , toString
  , toArray
  , toObject
  -- Encoding
  , fromNull
  , fromBoolean
  , fromNumber
  , fromString
  , fromArray
  , fromObject
  -- Defaults
  , jsonTrue
  , jsonFalse
  , jsonZero
  , jsonNull
  , jsonEmptyString
  , jsonEmptyArray
  , jsonEmptyObject
  , jsonSingletonArray
  , jsonSingletonObject
  -- Prisms
  , nullL
  , booleanL
  , numberL
  , stringL
  , arrayL
  , objectL
  -- Traversals
  , jsonNullL
  , jsonBooleanL
  , jsonNumberL
  , jsonStringL
  , jsonArrayL
  , jsonObjectL
  -- Decode
  , decodeJson
  , decodeMaybe
  , decodeL
  , arrayIndexL
  , objectFieldL
  -- Encode
  , encodeJson
  -- Parser
  , jsonParser
  -- Printer
  , printJson
  ) where

  import qualified Data.Argonaut.Combinators as Combinators
  import qualified Data.Argonaut.Core as Core
  import qualified Data.Argonaut.Decode as Decode
  import qualified Data.Argonaut.Encode as Encode
  import qualified Data.Argonaut.Parser as Parser
  import qualified Data.Argonaut.Printer as Printer

  -- | Combinators

  infix  7 :=
  infixr 6 ~>
  infixl 1 ?>>=

  (:=)   = Combinators.(:=)
  (~>)   = Combinators.(~>)
  (?>>=) = Combinators.(?>>=)

  -- | Core

  -- Types
  type Json     = Core.Json
  type JNull    = Core.JNull
  type JBoolean = Core.JBoolean
  type JNumber  = Core.JNumber
  type JString  = Core.JString
  type JArray   = Core.JArray
  type JObject  = Core.JObject


  -- Folds
  foldJson            = Core.foldJson
  foldJsonNull        = Core.foldJsonNull
  foldJsonBoolean     = Core.foldJsonBoolean
  foldJsonNumber      = Core.foldJsonNumber
  foldJsonString      = Core.foldJsonString
  foldJsonArray       = Core.foldJsonArray
  foldJsonObject      = Core.foldJsonObject

  -- Tests
  isNull              = Core.isNull
  isBoolean           = Core.isBoolean
  isNumber            = Core.isNumber
  isString            = Core.isString
  isArray             = Core.isArray
  isObject            = Core.isObject

  -- Decoding
  toNull              = Core.toNull
  toBoolean           = Core.toBoolean
  toNumber            = Core.toNumber
  toString            = Core.toString
  toArray             = Core.toArray
  toObject            = Core.toObject

  -- Encoding
  fromNull            = Core.fromNull
  fromBoolean         = Core.fromBoolean
  fromNumber          = Core.fromNumber
  fromString          = Core.fromString
  fromArray           = Core.fromArray
  fromObject          = Core.fromObject

  -- Defaults
  jsonTrue            = Core.jsonTrue
  jsonFalse           = Core.jsonFalse
  jsonZero            = Core.jsonZero
  jsonNull            = Core.jsonNull
  jsonEmptyString     = Core.jsonEmptyString
  jsonEmptyArray      = Core.jsonEmptyArray
  jsonEmptyObject     = Core.jsonEmptyObject
  jsonSingletonArray  = Core.jsonSingletonArray
  jsonSingletonObject = Core.jsonSingletonObject

  -- Prisms
  nullL               = Core.nullL
  booleanL            = Core.booleanL
  numberL             = Core.numberL
  stringL             = Core.stringL
  arrayL              = Core.arrayL
  objectL             = Core.objectL

  -- Traversals
  jsonNullL           = Core.jsonNullL
  jsonBooleanL        = Core.jsonBooleanL
  jsonNumberL         = Core.jsonNumberL
  jsonStringL         = Core.jsonStringL
  jsonArrayL          = Core.jsonArrayL
  jsonObjectL         = Core.jsonObjectL

  -- | Decode

  decodeJson   = Decode.decodeJson
  decodeMaybe  = Decode.decodeMaybe
  -- Lenses
  decodeL      = Decode.decodeL
  arrayIndexL  = Decode.arrayIndexL
  objectFieldL = Decode.objectFieldL

  -- | Encode

  encodeJson = Encode.encodeJson

  -- | Parser

  jsonParser = Parser.jsonParser

  -- | Printer

  printJson = Printer.printJson
