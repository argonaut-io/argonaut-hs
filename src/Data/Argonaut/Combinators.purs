module Data.Argonaut.Combinators
  ( (:=)
  , (~>)
  , (?>>=)
  ) where

  import Data.Argonaut.Core
    ( foldJsonObject
    , fromObject
    , jsonSingletonObject
    , Json()
    , JAssoc()
    )
  import Data.Argonaut.Encode (encodeJson, EncodeJson)
  import Data.Either (Either(..))
  import Data.Maybe (Maybe(..))
  import Data.Tuple (Tuple(..))

  import qualified Data.StrMap as M

  infix  7 :=
  infixr 6 ~>
  infixl 1 ?>>=

  (:=) :: forall a. (EncodeJson a) => String -> a -> JAssoc
  (:=) k v = Tuple k $ encodeJson v

  (~>) :: JAssoc -> Json -> Json
  (~>) (Tuple k v) = foldJsonObject (jsonSingletonObject k v) (M.insert k v >>> fromObject)

  (?>>=) :: forall a b. Maybe a -> String -> Either String a
  (?>>=) (Just x) _   = Right x
  (?>>=) _        str = Left $ "Couldn't decode " ++ str
