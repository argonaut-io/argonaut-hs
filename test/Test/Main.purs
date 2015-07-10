module Test.Main where

import Prelude

import Data.Argonaut
import Data.Argonaut.JCursor
import Data.Argonaut.Core (Json())
import Data.Either
import Data.Tuple
import Data.Maybe
import Data.Array 
import Data.List (toList)
import Control.Monad.Eff.Console
import qualified Data.StrMap as M

import Test.StrongCheck
import Test.StrongCheck.Gen

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
  return $  let f (AlphaNumString s) = s ++ "x"
                k' = f <$> k
            in  fromObject <<< M.fromList <<< toList <<< nubBy (\a b -> (fst a) == (fst b)) $ zipWith Tuple k' v

genJson :: Size -> Gen Json
genJson 0 = oneOf genJNull [genJBool, genJNumber, genJString]
genJson n = frequency (Tuple 1.0 genJNull) rest where
  rest = toList [Tuple 2.0 genJBool,
                 Tuple 2.0 genJNumber,
                 Tuple 3.0 genJString,
                 Tuple 1.0 (genJArray n),
                 Tuple 1.0 (genJObject n)]

instance arbitraryJson :: Arbitrary Json where
  arbitrary = sized genJson

prop_encode_then_decode :: Json -> Boolean
prop_encode_then_decode json =
  Right json == (decodeJson $ encodeJson $ json)

prop_decode_then_encode :: Json -> Boolean
prop_decode_then_encode json =
  let decoded = (decodeJson json) :: Either String Json in
  Right json == (decoded >>= (encodeJson >>> pure))

prop_toPrims_fromPrims :: Json -> Result
prop_toPrims_fromPrims j = Just j == fromPrims (toPrims j) <?> "fromPrims.toPrims: " ++ show (toPrims j) ++ "\n\n" ++ show (fromPrims (toPrims j))

instance arbJCursor :: Arbitrary JCursor where
  arbitrary =  do i <- chooseInt 0.0 2.0
                  r <- if i == 0 then pure JCursorTop 
                       else if i == 1 then JField <$> arbitrary <*> arbitrary
                            else JIndex <$> arbitrary <*> arbitrary
                  return r 

prop_jcursor_serialization :: JCursor -> Result
prop_jcursor_serialization c = 
  (decodeJson (encodeJson c) == Right c) <?> "JCursor: " ++ show c

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
  assert $  let bar = fromString "bar" 
            in  (M.singleton "foo" bar) .? "foo" == Right bar

