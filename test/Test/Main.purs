module Test.Main where

import Prelude

import Control.Monad.Eff.Console (log)

import Data.Argonaut (Json, decodeJson, encodeJson, fromString, (.?))
import Data.Argonaut.JCursor (JCursor(..), toPrims, fromPrims)
import Data.Argonaut.Gen (genJson)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.StrMap as M

import Test.StrongCheck (SC, Result, assert, quickCheck', (<?>))
import Test.StrongCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.StrongCheck.Gen (chooseInt, resize)

newtype TestJson = TestJson Json

instance arbitraryJson :: Arbitrary TestJson where
  arbitrary = TestJson <$> (resize 5 genJson)

prop_encode_then_decode :: TestJson -> Boolean
prop_encode_then_decode (TestJson json) =
  Right json == (decodeJson $ encodeJson $ json)

prop_decode_then_encode :: TestJson -> Boolean
prop_decode_then_encode (TestJson json) =
  let decoded = (decodeJson json) :: Either String Json in
  Right json == (decoded >>= (encodeJson >>> pure))

prop_toPrims_fromPrims :: TestJson -> Result
prop_toPrims_fromPrims (TestJson j) =
  Just j == fromPrims (toPrims j) <?> "fromPrims.toPrims: " <> show (toPrims j) <> "\n\n" <> show (fromPrims (toPrims j))

newtype TestJCursor = TestJCursor JCursor

runTestJCursor :: TestJCursor -> JCursor
runTestJCursor (TestJCursor j) = j

instance arbJCursor :: Arbitrary TestJCursor where
  arbitrary = do
    i <- chooseInt 0 2
    r <- if i == 0 then pure JCursorTop
         else if i == 1 then JField <$> arbitrary <*> (runTestJCursor <$> arbitrary)
              else JIndex <$> arbitrary <*> (runTestJCursor <$> arbitrary)
    pure $ TestJCursor r

prop_jcursor_serialization :: TestJCursor -> Result
prop_jcursor_serialization (TestJCursor c) =
  (decodeJson (encodeJson c) == Right c) <?> "JCursor: " <> show c

main :: SC () Unit
main = do
  log "Testing that any JSON can be encoded and then decoded"
  quickCheck' 20 prop_encode_then_decode

  log "Testing that any JSON can be decoded and then encoded"
  quickCheck' 20 prop_decode_then_encode

  log "Testing that toPrims / fromPrims inverses"
  quickCheck' 20 prop_toPrims_fromPrims

  log "Testing that JCursor can be encoded / decoded from JSON"
  quickCheck' 20 prop_jcursor_serialization

  log "Testing .? combinator"
  assert $ let bar = fromString "bar"
           in  (M.singleton "foo" bar) .? "foo" == Right bar
