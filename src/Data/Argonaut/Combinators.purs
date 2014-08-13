module Data.Argonaut.Combinators
  ( (:=)
  , (~>)
  ) where

  import Data.Argonaut.Core
    ( foldJsonObject
    , fromObject
    , jsonSingletonObject
    , Json()
    , JAssoc()
    )
  import Data.Argonaut.Encode (encodeJson, EncodeJson)
  import Data.Tuple (Tuple(..))

  import qualified Data.Map as M

  infix 7 :=
  infixr 6 ~>

  (:=) :: forall a. (EncodeJson a) => String -> a -> JAssoc
  (:=) k v = Tuple k $ encodeJson v

  (~>) :: JAssoc -> Json -> Json
  (~>) (Tuple k v) = foldJsonObject (jsonSingletonObject k v) (M.insert k v >>> fromObject)
