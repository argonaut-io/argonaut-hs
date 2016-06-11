module Examples.Data.Argonaut.Record where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)

import Data.Argonaut (class EncodeJson, class DecodeJson, encodeJson, printJson, jsonEmptyObject, decodeJson, (~>), (:=), (.?))
import Data.Maybe (Maybe(..))

newtype Foo = Foo
  { foo :: Maybe Int
  , bar :: Maybe String
  }

instance decodeJsonFoo :: DecodeJson Foo where
  decodeJson json = do
    obj <- decodeJson json
    foo <- obj .? "foo"
    bar <- obj .? "bar"
    pure $ Foo { foo, bar}

instance encodeJsonFoo :: EncodeJson Foo where
  encodeJson (Foo { foo, bar })
    =  "bar" := bar
    ~> "foo" := foo
    ~> jsonEmptyObject

instance showFoo :: Show Foo where
  show (Foo { foo, bar }) =
    "Foo { foo: " <> show foo <> ", bar: " <> show bar <> " }"

main :: Eff (console :: CONSOLE) Unit
main = do
  logShow $ "raw foo is: " <> show foo
  logShow $ "encoded foo is: " <> printJson (encodeJson foo)
  where
  foo :: Foo
  foo = Foo { foo: Just 42, bar: Nothing }
