module Data.Argonaut.Decode
  ( DecodeJson
  , decodeJson
  , decodeMaybe
  -- Lenses
  , decodeL
  , arrayIndexL
  , objectFieldL
  ) where

  import Control.Lens
    ( ix
    , itraversed
    , prism'
    , IndexedTraversalP()
    , PrismP()
    , TraversalP()
    )

  import Data.Argonaut.Core
    ( Json()
    , JNumber()
    , JString()
    , arrayL
    , foldJsonNull
    , foldJsonBoolean
    , foldJsonNumber
    , foldJsonString
    , foldJsonArray
    , foldJsonObject
    , objectL
    , toObject
    , toString
    , toNumber
    )
  import Data.Argonaut.Encode (encodeJson, EncodeJson)
  import Data.Either (either, Either(..))
  import Data.Maybe (maybe, Maybe(..))

  import qualified Data.Map as M

  class DecodeJson a where
    decodeJson :: Json -> Either String a

  instance decodeJsonNull :: DecodeJson Unit where
    decodeJson = foldJsonNull (Left "Not null.") Right

  instance decodeJsonBoolean :: DecodeJson Boolean where
    decodeJson = foldJsonBoolean (Left "Not a Boolean.") Right

  instance decodeJsonNumber :: DecodeJson Number where
    decodeJson = foldJsonNumber (Left "Not a Number.") Right

  instance decodeJsonString :: DecodeJson String where
    decodeJson = foldJsonString (Left "Not a String.") Right

  instance decodeJsonArray :: DecodeJson [Json] where
    decodeJson = foldJsonArray (Left "Not a Array.") Right

  instance decodeJsonObject :: DecodeJson (M.Map String Json) where
    decodeJson = foldJsonObject (Left "Not a Object.") Right

  instance decodeJsonJson :: DecodeJson Json where
    decodeJson = Right

  decodeMaybe :: forall a. (DecodeJson a) => Json -> Maybe a
  decodeMaybe json = decodeJson json # either ((const Nothing) :: forall a. String -> Maybe a) Just

  decodeL :: forall a. (DecodeJson a, EncodeJson a) => PrismP Json a
  decodeL = prism' encodeJson decodeMaybe

  arrayIndexL :: forall a. (DecodeJson a, EncodeJson a) => JNumber -> TraversalP Json a
  arrayIndexL i = decodeL >>> ix i >>> arrayL

  objectFieldL :: forall a. (DecodeJson a, EncodeJson a) => JString -> TraversalP Json a
  objectFieldL key = decodeL >>> ix key >>> objectL

  -- objectMembersL :: forall a. (DecodeJson a, EncodeJson a) => IndexedTraversalP JString Json a
  -- objectMembersL = decodeL >>> itraversed >>> objectL

  -- arrayMembersL :: forall a. (DecodeJson a, EncodeJson a) => IndexedTraversalP JNumber Json a
  -- arrayMembersL = decodeL >>> traversed >>> arrayL
