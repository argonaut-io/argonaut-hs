module Data.Argonaut.Decode where

  import Control.Lens
    ( ix
    , itraversed
    , prism'
    , IndexedTraversalP()
    , PrismP()
    , TraversalP()
    )
  import Control.Monad.Identity (runIdentity, Identity(..))

  import Data.Argonaut.Core
    ( Json()
    , JNumber()
    , JString()
    , Foo(..)
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
  import Data.Argonaut.Encode (encodeIdentity, EncodeJson)
  import Data.Either (either, Either(..))
  import Data.Maybe (maybe, Maybe(..))

  import qualified Data.Map as M

  class DecodeJson m n a where
    decodeJson :: m Json -> n a

  instance decodeJsonIdESDRNull :: DecodeJson Identity (Either String) Unit where
    decodeJson = runIdentity >>> foldJsonNull (Left "Not null.") Right

  instance decodeJsonIdESDRBoolean :: DecodeJson Identity (Either String) Boolean where
    decodeJson = runIdentity >>> foldJsonBoolean (Left "Not a Boolean.") Right

  instance decodeJsonIdESDRNumber :: DecodeJson Identity (Either String) Number where
    decodeJson = runIdentity >>> foldJsonNumber (Left "Not a Number.") Right

  instance decodeJsonIdESDRString :: DecodeJson Identity (Either String) String where
    decodeJson = runIdentity >>> foldJsonString (Left "Not a String.") Right

  instance decodeJsonIdESDRArray :: DecodeJson Identity (Either String) [Json] where
    decodeJson = runIdentity >>> foldJsonArray (Left "Not a Array.") Right

  instance decodeJsonIdESDRObject :: DecodeJson Identity (Either String) (M.Map String Json) where
    decodeJson = runIdentity >>> foldJsonObject (Left "Not a Object.") Right

  instance decodeJsonIdESDRJson :: DecodeJson Identity (Either String) Json where
    decodeJson = runIdentity >>> Right

  decodeMaybe :: forall a. (DecodeJson Identity (Either String) a) => Json -> Maybe a
  decodeMaybe = Identity >>> decodeJson >>> either ((const Nothing) :: forall a. String -> Maybe a) Just

  decodeL :: forall a. (DecodeJson Identity (Either String) a, EncodeJson Identity Identity a) => PrismP Json a
  decodeL = prism' encodeIdentity decodeMaybe

  arrayIndexL :: forall a. (DecodeJson Identity (Either String) a, EncodeJson Identity Identity a) => JNumber -> TraversalP Json a
  arrayIndexL i = decodeL >>> ix i >>> arrayL

  objectFieldL :: forall a. (DecodeJson Identity (Either String) a, EncodeJson Identity Identity a) => JString -> TraversalP Json a
  objectFieldL key = decodeL >>> ix key >>> objectL

  -- objectMembersL :: forall a. (DecodeJson Identity (Either String) a, EncodeJson Identity Identity a) => IndexedTraversalP JString Json a
  -- objectMembersL = decodeL >>> itraversed >>> objectL

  -- arrayMembersL :: forall a. (DecodeJson Identity (Either String) a, EncodeJson Identity Identity a) => IndexedTraversalP JNumber Json a
  -- arrayMembersL = decodeL >>> traversed >>> arrayL

  instance decodeFoo :: DecodeJson Identity (Either String) Foo where
    decodeJson (Identity json) = maybe (Left "Not a Foo.") Right $ do
      obj <- toObject json
      foo <- (M.lookup "foo" obj >>= toString)
      bar <- (M.lookup "bar" obj >>= toNumber)
      pure (Foo {foo: foo, bar: bar})
