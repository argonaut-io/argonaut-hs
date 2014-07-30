module Data.Argonaut.Combinators where

  import Control.Lens (_1, _2)
  import Control.Monad.Identity (Identity())

  import Data.Argonaut.Core
    ( foldJsonObject
    , fromObject
    , jsonSingletonObject
    , Json()
    , JAssoc()
    , JField()
    )
  import Data.Argonaut.Encode (encodeIdentity, EncodeJson)
  import Data.Tuple (Tuple(..))

  import qualified Data.Map as M

  infixl 4 :=
  infixr 6 ~>

  (:=) :: forall a. (EncodeJson Identity Identity a) => String -> a -> JAssoc
  (:=) key val = Tuple key $ encodeIdentity val

  (~>) :: JAssoc -> Json -> Json
  (~>) (Tuple k v) = foldJsonObject (jsonSingletonObject k v) (M.insert k v >>> fromObject)
