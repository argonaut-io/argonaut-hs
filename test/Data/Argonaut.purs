module Test.Data.Argonaut where

  import Data.Argonaut
  import Data.Argonaut.Core (Json())
  import Data.Either

  import Test.QuickCheck
  import Test.QuickCheck.LCG

  instance arbitraryJson :: Arbitrary Json where
    arbitrary = do
      n <- (*) 10 <$> arbitrary
      pure $ if n < 5 then
          jsonNull
        else if n < 10 then
          jsonTrue
        else
          jsonFalse

  prop_encode_then_decode :: Json -> Boolean
  prop_encode_then_decode json =
    Right json == (decodeJson $ encodeJson $ json)

  prop_decode_then_encode :: Json -> Boolean
  prop_decode_then_encode json =
    let decoded = (decodeJson json) :: Either String Json in
    Right json == (decoded >>= (encodeJson >>> pure))

  main = do
    quickCheck prop_encode_then_decode
    quickCheck prop_decode_then_encode
