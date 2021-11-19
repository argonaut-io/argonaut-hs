module Test.Main where

import Prelude

import Data.Argonaut (Json, decodeJson, encodeJson, stringify)
import Data.Argonaut.Gen (genJson)
import Data.Argonaut.JCursor (JCursor(..), toPrims, fromPrims)
import Data.Either (Either(..))
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..))
import Data.String.Gen (genUnicodeString)
import Effect (Effect)
import Effect.Console (log)
import Test.QuickCheck (Result, quickCheck, (<?>))
import Test.QuickCheck.Gen (Gen, chooseInt, resize)

genTestJson :: Gen Json
genTestJson = resize 5 genJson

prop_toPrims_fromPrims :: Gen Result
prop_toPrims_fromPrims = do
  j <- genTestJson
  pure $ Just j == fromPrims (toPrims j) <?> "fromPrims.toPrims: " <> show (toPrims j) <> "\n\n" <> foldMap stringify (fromPrims (toPrims j))

genTestJCursor :: Gen JCursor
genTestJCursor = do
  i <- chooseInt 0 2
  r <-
    if i == 0 then pure JCursorTop
    else if i == 1 then JField <$> genUnicodeString <*> genTestJCursor
    else JIndex <$> chooseInt bottom top <*> genTestJCursor
  pure r

prop_jcursor_serialization :: Gen Result
prop_jcursor_serialization = do
  c <- genTestJCursor
  pure $ (decodeJson (encodeJson c) == Right c) <?> "JCursor: " <> show c

main :: Effect Unit
main = do
  log "Testing that toPrims / fromPrims inverses"
  quickCheck prop_toPrims_fromPrims

  log "Testing that JCursor can be encoded / decoded from JSON"
  quickCheck prop_jcursor_serialization
