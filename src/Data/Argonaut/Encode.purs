module Data.Argonaut.Encode
  ( EncodeJson
  , encodeJson
  ) where

  import Data.Argonaut.Core
    ( Json(..)
    , foldJsonObject
    , fromNull
    , fromBoolean
    , fromNumber
    , fromString
    , fromArray
    , fromObject
    , jsonEmptyArray
    , jsonEmptyObject
    , jsonSingletonObject
    )
  import Data.Foldable (foldr)
  import Data.Tuple (Tuple(..))

  import qualified Data.Map as M

  class EncodeJson a where
    encodeJson :: a -> Json

  instance encodeJsonJNull :: EncodeJson Unit where
    encodeJson = fromNull

  instance encodeJsonJBoolean :: EncodeJson Boolean where
    encodeJson = fromBoolean

  instance encodeJsonJNumber :: EncodeJson Number where
    encodeJson = fromNumber

  instance encodeJsonJString :: EncodeJson String where
    encodeJson = fromString

  instance encodeJsonJson :: EncodeJson Json where
    encodeJson = id

  instance encodeJsonArray :: (EncodeJson a) => EncodeJson [a] where
    encodeJson json = fromArray (encodeJson <$> json)

  instance encodeMap :: (EncodeJson a) => EncodeJson (M.Map String a) where
    encodeJson json = foldr append jsonEmptyObject (assoc <$> M.toList json)
      where
        append (Tuple k v) =
          foldJsonObject (jsonSingletonObject k v) (M.insert k v >>> fromObject)
        assoc (Tuple k v) = Tuple k $ encodeJson v
