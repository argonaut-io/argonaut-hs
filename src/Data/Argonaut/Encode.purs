module Data.Argonaut.Encode
  ( EncodeJson
  , encodeJson
  ) where

  import Data.Argonaut.Core
    ( Json(..)
    , fromNull
    , fromBoolean
    , fromNumber
    , fromString
    , fromArray
    , fromObject
    )

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

  instance encodeJsonJArray :: EncodeJson [Json] where
    encodeJson = fromArray

  instance encodeJsonJObject :: EncodeJson (M.Map String Json) where
    encodeJson = fromObject

  instance encodeJsonJson :: EncodeJson Json where
    encodeJson = id
