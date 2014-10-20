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
    , toArray
    , toNumber
    , toObject
    , toString
    )
  import Data.Argonaut.Encode (encodeJson, EncodeJson)
  import Data.Either (either, Either(..))
  import Data.Maybe (maybe, Maybe(..))
  import Data.Foldable (Foldable, foldl, foldMap, foldr)
  import Data.Traversable (Traversable, traverse)
  import Data.Tuple (uncurry)

  import qualified Data.StrMap as M

  class DecodeJson a where
    decodeJson :: Json -> Either String a

  instance decodeJsonNull :: DecodeJson Unit where
    decodeJson = foldJsonNull (Left "Not null.") (const $ Right unit)

  instance decodeJsonBoolean :: DecodeJson Boolean where
    decodeJson = foldJsonBoolean (Left "Not a Boolean.") Right

  instance decodeJsonNumber :: DecodeJson Number where
    decodeJson = foldJsonNumber (Left "Not a Number.") Right

  instance decodeJsonString :: DecodeJson String where
    decodeJson = foldJsonString (Left "Not a String.") Right

  instance decodeJsonArray :: DecodeJson [Json] where
    decodeJson = foldJsonArray (Left "Not an Array.") Right

  instance decodeJsonJson :: DecodeJson Json where
    decodeJson = Right

  instance decodeMap :: (DecodeJson a) => DecodeJson (M.StrMap a) where
    decodeJson json = maybe (Left "Couldn't decode.") Right $ do
      obj <- toObject json
      traverse decodeMaybe obj

  instance decodeArray :: (DecodeJson a) => DecodeJson [a] where
    decodeJson json = maybe (Left "Couldn't decode.") Right $ do
      obj <- toArray json
      traverse decodeMaybe obj

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

  -- Orphans

  -- Should move these orphans to purescript-foldable-traversable.
  instance traversableMap :: Traversable M.StrMap where
    traverse f ms = foldr (\x acc -> M.union <$> x <*> acc) (pure M.empty) ((\fs -> uncurry M.singleton <$> fs) <$> (traverse f <$> M.toList ms))
    sequence = traverse id
