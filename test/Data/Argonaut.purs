module Test.Data.Argonaut where

  import Data.Argonaut
  import Data.Argonaut.JCursor
  import Data.Argonaut.Core (Json())
  import Data.Either
  import Data.Tuple
  import Data.Maybe
  import Data.Array
  import Debug.Trace
  import qualified Data.StrMap as M

  import Test.QuickCheck
  import Test.QuickCheck.LCG

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
              in  fromObject <<< M.fromList <<< nubBy (\a b -> (fst a) == (fst b)) $ zipWith Tuple k' v

  genJson :: Size -> Gen Json
  genJson 0 = oneOf genJNull [genJBool, genJNumber, genJString]
  genJson n = frequency (Tuple 1 genJNull) rest where
    rest = [Tuple 2 genJBool,
            Tuple 2 genJNumber,
            Tuple 3 genJString,
            Tuple 1 (genJArray n),
            Tuple 1 (genJObject n)]

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
    arbitrary =  do i <- chooseInt 0 2
                    r <- if i == 0 then pure JCursorTop 
                         else if i == 1 then JField <$> arbitrary <*> arbitrary
                         else JIndex <$> arbitrary <*> arbitrary
                    return r 

  prop_jcursor_serialization :: JCursor -> Result
  prop_jcursor_serialization c = 
    (decodeJson (encodeJson c) == Right c) <?> "JCursor: " ++ show c

  main = do
    trace "Showing small sample of JSON"
    showSample (genJson 10)

    trace "Testing that any JSON can be encoded and then decoded"
    quickCheck prop_encode_then_decode

    trace "Testing that any JSON can be decoded and then encoded"
    quickCheck prop_decode_then_encode

    trace "Testing that toPrims / fromPrims inverses"
    quickCheck prop_toPrims_fromPrims

    trace "Testing that JCursor can be encoded / decoded from JSON"
    quickCheck prop_jcursor_serialization
