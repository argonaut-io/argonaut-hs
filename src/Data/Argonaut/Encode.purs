module Data.Argonaut.Encode where

  import Control.Monad.Identity (runIdentity, Identity(..))
  import Data.Argonaut.Core
    ( Json(..)
    , Foo(..)
    , fromNull
    , fromBoolean
    , fromNumber
    , fromString
    , fromArray
    , fromObject
    )
  import Data.Tuple (Tuple(..))

  import qualified Data.Map as M

  class EncodeJson m n a where
    encodeJson :: m a -> n Json

  instance encodeJsonIdIdJNull :: EncodeJson Identity Identity Unit where
    encodeJson = runIdentity >>> fromNull >>> Identity

  instance encodeJsonIdIdJBoolean :: EncodeJson Identity Identity Boolean where
    encodeJson = runIdentity >>> fromBoolean >>> Identity

  instance encodeJsonIdIdJNumber :: EncodeJson Identity Identity Number where
    encodeJson = runIdentity >>> fromNumber >>> Identity

  instance encodeJsonIdIdJString :: EncodeJson Identity Identity String where
    encodeJson = runIdentity >>> fromString >>> Identity

  instance encodeJsonIdIdJArray :: EncodeJson Identity Identity [Json] where
    encodeJson = runIdentity >>> fromArray >>> Identity

  instance encodeJsonIdIdJObject :: EncodeJson Identity Identity (M.Map String Json) where
    encodeJson = runIdentity >>> fromObject >>> Identity

  instance encodeJsonIdIdJson :: EncodeJson Identity Identity Json where
    encodeJson = runIdentity >>> Identity

  encodeTo :: forall m a n. (EncodeJson m n a) => m a -> n Json
  encodeTo = encodeJson

  encodeIdentity :: forall a. (EncodeJson Identity Identity a) => a -> Json
  encodeIdentity = Identity >>> encodeTo >>> runIdentity

  instance showFoo :: Show Foo where
    show (Foo f) = "Foo(" ++ show f.foo ++ ", " ++ show f.bar ++ ")"

  instance encodeFoo :: EncodeJson Identity Identity Foo where
    encodeJson (Identity (Foo {foo = f, bar = b})) = Identity $ JsonObject $
      M.fromList [Tuple "foo" $ JsonString f, Tuple "bar" $ JsonNumber b]
