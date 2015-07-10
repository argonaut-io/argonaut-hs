module Data.Argonaut.JCursor 
  ( JCursor(..)
  , JsonPrim(..)
  , cursorGet
  , cursorSet
  , fromPrims
  , downField
  , downIndex
  , insideOut
  , primNull
  , primBool
  , primNum
  , primStr
  , primToJson
  , runJsonPrim
  , toPrims
  ) where

import Prelude

import Data.Argonaut.Core
import Data.Argonaut.Combinators
import Data.Argonaut.Encode
import Data.Argonaut.Decode
import Data.Maybe
import Data.Tuple
import Data.Foldable
import Data.Monoid
--import Data.String 
import Data.List (List(), zipWith, range, head, singleton, toList)
import Data.Either(Either(..))
import qualified Data.Array as A
import qualified Data.StrMap as M
import qualified Data.Int as I
import qualified Data.Maybe.Unsafe as MU
  
data JCursor = JCursorTop | JField String JCursor | JIndex Int JCursor

newtype JsonPrim = JsonPrim (forall a. (JNull -> a) -> (JBoolean -> a) -> (JNumber -> a) -> (JString -> a) -> a)

runJsonPrim :: JsonPrim -> (forall a. (JNull -> a) -> (JBoolean -> a) -> (JNumber -> a) -> (JString -> a) -> a)
runJsonPrim (JsonPrim p) = p

foreign import jsonNull0 :: JNull

primNull :: JsonPrim
primNull = JsonPrim (\f _ _ _ -> f jsonNull0)

primBool :: JBoolean -> JsonPrim
primBool v = JsonPrim (\_ f _ _ -> f v)

primNum :: JNumber -> JsonPrim
primNum v = JsonPrim (\_ _ f _ -> f v)

primStr :: JString -> JsonPrim
primStr v = JsonPrim (\_ _ _ f -> f v)

primToJson :: JsonPrim -> Json
primToJson p = runJsonPrim p fromNull fromBoolean fromNumber fromString

insideOut :: JCursor -> JCursor
insideOut JCursorTop = JCursorTop
insideOut (JField i c) = downField i (insideOut c)
insideOut (JIndex i c) = downIndex i (insideOut c)

downField :: String -> JCursor -> JCursor
downField i = downField' where
  downField' JCursorTop = JField i JCursorTop
  downField' (JField i' c) = JField i' (downField' c) 
  downField' (JIndex i' c) = JIndex i' (downField' c)

downIndex :: Int -> JCursor -> JCursor
downIndex i = downIndex' where
  downIndex' JCursorTop = JIndex i JCursorTop
  downIndex' (JField i' c) = JField i' (downIndex' c)
  downIndex' (JIndex i' c) = JIndex i' (downIndex' c)

cursorGet :: JCursor -> Json -> Maybe Json
cursorGet JCursorTop = Just
cursorGet (JField i c) = foldJsonObject Nothing g where
  g m = M.lookup i m >>= cursorGet c
cursorGet (JIndex i c) = foldJsonArray Nothing g where
  g a = a A.!! i >>= cursorGet c

inferEmpty :: JCursor -> Json
inferEmpty JCursorTop   = jsonNull
inferEmpty (JField _ _) = jsonEmptyObject
inferEmpty (JIndex _ _) = jsonEmptyArray

cursorSet :: JCursor -> Json -> Json -> Maybe Json
cursorSet JCursorTop   v = Just <<< const v
cursorSet (JField i c) v = foldJson (const d) (const d) (const d) (const d) (const d) g where
  d = fromObject <<< M.singleton i <$> cursorSet c v (inferEmpty c)
  g m = fromObject <<< flip (M.insert i) m <$> (cursorSet c v $ fromMaybe (inferEmpty c) (M.lookup i m))
cursorSet (JIndex i c) v = foldJson (const d) (const d) (const d) (const d) g (const d) where
  d = fromArray <<< MU.fromJust <<< flip (A.updateAt i) (const jsonNull <$> A.range 0 i) <$> cursorSet c v (inferEmpty c)
  g a = (cursorSet c v $ fromMaybe (inferEmpty c) (a A.!! i)) >>= setArr a i

  setArr xs i v = let len = A.length xs 
                  in  if i < 0
                      then Nothing 
                      else if i >= len 
                           then setArr (xs <> (const jsonNull <$> A.range 0 (i - len))) i v
                           else Just <<< fromArray <<< MU.fromJust $ A.updateAt i v xs

toPrims :: Json -> List (Tuple JCursor JsonPrim)
toPrims = foldJson  (const $ singleton $ Tuple JCursorTop primNull)
          (\b -> singleton $ Tuple JCursorTop $ primBool b)
          (\n -> singleton $ Tuple JCursorTop $ primNum n)
          (\s -> singleton $ Tuple JCursorTop $ primStr s)
          (\a -> let zipped :: List (Tuple Int Json)
                     zipped = zipWith Tuple (range 0 (A.length a - 1)) (toList a)

                     f :: Tuple Int Json -> List (Tuple JCursor JsonPrim)
                     f (Tuple i j) = toList ((\t -> Tuple (JIndex i (fst t)) (snd t)) <$> toPrims j)
                 in  zipped >>= f)
          (\o -> let f :: Tuple String Json -> List (Tuple JCursor JsonPrim)
                     f (Tuple i j) = toList ((\t -> Tuple (JField i (fst t)) (snd t)) <$> toPrims j)
                 in  M.toList o >>= f)

fromPrims :: List (Tuple JCursor JsonPrim) -> Maybe Json
fromPrims lst = foldl f (inferEmpty <<< fst <$> head lst) lst
  where
  f :: Maybe Json -> Tuple JCursor JsonPrim -> Maybe Json
  f j (Tuple c p) = j >>= cursorSet c (runJsonPrim p fromNull fromBoolean fromNumber fromString)

instance showJCursor :: Show JCursor where
  show JCursorTop = ""
  show (JField i c) = "." ++ i ++ show c
  show (JIndex i c) = "[" ++ show i ++ "]" ++ show c

instance showJsonPrim :: Show JsonPrim where
  show p = runJsonPrim p show show show show

instance eqJCursor :: Eq JCursor where
  eq JCursorTop JCursorTop = true
  eq (JField i1 c1) (JField i2 c2) = i1 == i2 && c1 == c2
  eq (JIndex i1 c1) (JIndex i2 c2) = i1 == i2 && c1 == c2
  eq _ _ = false

instance ordJCursor :: Ord JCursor where
  compare JCursorTop JCursorTop = EQ
  compare JCursorTop _ = LT
  compare _ JCursorTop = GT
  compare (JField _ _) (JIndex _ _) = LT
  compare (JIndex _ _) (JField _ _) = GT
  compare (JField i1 c1) (JField i2 c2) = case compare i1 i2 of
    EQ -> compare c1 c2
    x  -> x
  compare (JIndex i1 c1) (JIndex i2 c2) = case compare i1 i2 of
    EQ -> compare c1 c2
    x  -> x

instance semigroupJCursor :: Semigroup JCursor where
  append a JCursorTop = a
  append JCursorTop b = b
  append (JField i a) b = JField i (a <> b)
  append (JIndex i a) b = JIndex i (a <> b)

instance monoidJCursor :: Monoid JCursor where
  mempty = JCursorTop

instance encodeJsonJCursor :: EncodeJson JCursor where
  encodeJson = encodeJson <<< loop where
    loop JCursorTop = []
    loop (JField i c) = [encodeJson i] <> loop c
    loop (JIndex i c) = [encodeJson i] <> loop c

fail :: forall a b. (Show a) => a -> Either String b
fail x = Left $ "Expected String or Number but found: " ++ show x            

instance decodeJsonJCursor :: DecodeJson JCursor where
  decodeJson j = decodeJson j >>= loop
    where
    loop :: Array Json -> Either String JCursor
    loop arr =
      maybe (Right JCursorTop) goLoop do
        x <- A.head arr
        xs <- A.tail arr
        pure $ Tuple x xs

    goLoop :: Tuple Json (Array Json) -> Either String JCursor
    goLoop (Tuple x xs) = do
      c <- loop xs
      foldJson fail fail (goNum c) (Right <<< (flip JField c)) fail fail x

    goNum :: JCursor -> JNumber -> Either String JCursor
    goNum c num =
      maybe (Left "Not an Int") (Right <<< (flip JIndex c)) $ I.fromNumber num


