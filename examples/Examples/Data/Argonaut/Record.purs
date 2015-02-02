module Examples.Data.Argonaut.Record where

  import Data.Argonaut ((~>), (:=), jsonEmptyObject, printJson)
  import Data.Argonaut.Encode (EncodeJson, encodeJson)
  import Data.Maybe (Maybe(..))

  import Debug.Trace (print)

  newtype Foo = Foo
    { foo :: Maybe Number
    , bar :: Maybe String
    }

  instance encodeJsonFoo :: EncodeJson Foo where
    encodeJson (Foo f)
      =  "bar" := f.bar
      ~> "foo" := f.foo
      ~> jsonEmptyObject

  instance showFoo :: Show Foo where
    show (Foo f) = "Foo {foo: " ++ show f.foo ++ ", bar:" ++ show f.bar ++ "}"

  foo :: Foo
  foo = Foo {foo: Just 42, bar: Nothing}

  main = do
    print $ "raw foo is: " ++ show foo
    print $ "encoded foo is: " ++ printJson (encodeJson foo)
