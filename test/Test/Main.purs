module Test.Main where

import Prelude

import Control.Monad.Eff.Console (log)

import Data.Argonaut (Json, fromString, encodeJson, decodeJson, fromObject, fromArray, fromNumber, fromBoolean, jsonNull, (.?))
import Data.Argonaut.JCursor (JCursor(..), toPrims, fromPrims)
import Data.Array (zipWith, nubBy, length)
import Data.Either (Either(..))
import Data.List (fromFoldable)
import Data.Maybe (Maybe(..))
import Data.StrMap as M
import Data.Tuple (Tuple(..), fst)

import Test.StrongCheck (SC, Result, assert, quickCheck', (<?>))
import Test.StrongCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.StrongCheck.Data.AlphaNumString (AlphaNumString(..))
import Test.StrongCheck.Gen (Gen, Size, showSample, chooseInt, sized, frequency, oneOf, vectorOf)

newtype TestJson = TestJson Json

genJNull :: Gen Json
genJNull = pure jsonNull

genJBool :: Gen Json
genJBool = fromBoolean <$> arbitrary

genJNumber :: Gen Json
genJNumber = fromNumber <$> arbitrary

genJString :: Gen Json
genJString = fromString <$> arbitrary

genJArray :: Size -> Gen Json
genJArray sz = fromArray <$> vectorOf sz (genJson $ sz - 1)

genJObject :: Size -> Gen Json
genJObject sz = do
  v <- vectorOf sz (genJson $ sz - 1)
  k <- vectorOf (length v) (arbitrary :: Gen AlphaNumString)
  let
    f (AlphaNumString s) = s <> "x"
    k' = f <$> k
  pure $ fromObject <<< M.fromFoldable <<< nubBy (\a b -> (fst a) == (fst b)) $ zipWith Tuple k' v

genJson :: Size -> Gen Json
genJson 0 = oneOf genJNull [genJBool, genJNumber, genJString]
genJson n = frequency (Tuple 1.0 genJNull) rest where
  rest = fromFoldable [Tuple 2.0 genJBool,
                 Tuple 2.0 genJNumber,
                 Tuple 3.0 genJString,
                 Tuple 1.0 (genJArray n),
                 Tuple 1.0 (genJObject n)]


instance arbitraryJson :: Arbitrary TestJson where
  arbitrary = TestJson <$> sized genJson

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
  log "Showing small sample of JSON"
  showSample (genJson 10)

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
