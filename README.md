# PureScript-Argonaut

__Warning__: This currently requires the branch [508][508] in order to compile.

This is an implementation of [Argonaut][argonaut] for the PureScript language.

While not as feature rich as the Scala version, it still supports encoding and decoding.

## Installation

This can be installed with bower:

```shell
bower i purescript-argonaut
```

## Usage

If you have some data type, and you want to encode it into JSON,
you need to define an instance for Encode.

If you have some JSON, and you want to decode it into some data type,
you need to define an instance for Decode.

## Examples

```purescript
module Foo where

  import Control.Identity
  import Data.Argonaut
  import Data.Either

  data Foo = Foo
    { foo :: String
    , bar :: Number
    }

  instance showFoo :: Show Foo where
    show (Foo f) = "Foo(" ++ show f.foo ++ ", " ++ show f.bar ++ ")"

  instance decodeFoo :: DecodeJson Identity (Either String) Foo where
    decodeJson (Identity json) = maybe (Left "Not a Foo.") Right $ do
      obj <- toObject json
      foo <- (M.lookup "foo" obj >>= toString)
      bar <- (M.lookup "bar" obj >>= toNumber)
      pure (Foo {foo: foo, bar: bar})

  instance encodeFoo :: EncodeJson Identity Identity Foo where
    encodeJson (Identity (Foo {foo = f, bar = b})) = Identity $ JsonObject $
      M.fromList [Tuple "foo" $ JsonString f, Tuple "bar" $ JsonNumber b]
```

[508]: https://github.com/purescript/purescript/tree/508
[argonaut]: http://argonaut.io/
