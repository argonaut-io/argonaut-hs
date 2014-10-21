module Data.Argonaut.JCursor 
  ( JCursor(..)
  , JsonPrim(..)
  , cursorGet
  , cursorSet
  , fromPrims
  , downField
  , downIndex
  , insideOut
  , runJsonPrim
  , toPrims
  ) where

  import Data.Argonaut.Core
  import Data.Maybe
  import Data.Tuple
  import Data.Foldable
  import Data.Monoid
  import qualified Data.Array as A
  import qualified Data.StrMap as M
  
  data JCursor = JCursorTop | JField String JCursor | JIndex Number JCursor

  newtype JsonPrim = JsonPrim (forall a. (JNull -> a) -> (JBoolean -> a) -> (JNumber -> a) -> (JString -> a) -> a)

  runJsonPrim :: JsonPrim -> (forall a. (JNull -> a) -> (JBoolean -> a) -> (JNumber -> a) -> (JString -> a) -> a)
  runJsonPrim (JsonPrim p) = p

  foreign import jsonNull0 "var jsonNull0 = null;" :: JNull

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

  downIndex :: Number -> JCursor -> JCursor
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
    d = fromArray <<< flip (A.updateAt i) (const jsonNull <$> A.range 0 i) <$> cursorSet c v (inferEmpty c)
    g a = (cursorSet c v $ fromMaybe (inferEmpty c) (a A.!! i)) >>= setArr a i

    setArr xs i v = let len = A.length xs 
                    in  if i < 0 then
                          Nothing 
                        else if i >= len then
                          setArr (xs <> (const jsonNull <$> A.range 0 (i - len))) i v
                        else Just <<< fromArray $ A.updateAt i v xs

  toPrims :: Json -> [Tuple JCursor JsonPrim]
  toPrims = foldJson  (const [Tuple JCursorTop primNull])
                      (\b -> [Tuple JCursorTop $ primBool b])
                      (\n -> [Tuple JCursorTop $ primNum n])
                      (\s -> [Tuple JCursorTop $ primStr s])
                      (\a -> let zipped = A.zipWith Tuple (A.range 0 (A.length a - 1)) a

                                 f (Tuple i j) = (\t -> Tuple (JIndex i (fst t)) (snd t)) <$> toPrims j 

                             in  zipped >>= f)
                      (\o -> let f (Tuple i j) = (\t -> Tuple (JField i (fst t)) (snd t)) <$> toPrims j
                             in  M.toList o >>= f)

  fromPrims :: [Tuple JCursor JsonPrim] -> Maybe Json
  fromPrims a = foldlArray f (inferEmpty <<< fst <$> A.head a) a where
    f :: Maybe Json -> Tuple JCursor JsonPrim -> Maybe Json
    f j (Tuple c p) = j >>= cursorSet c (runJsonPrim p fromNull fromBoolean fromNumber fromString)

  instance showJCursor :: Show JCursor where
    show JCursorTop = ""
    show (JField i c) = "." ++ i ++ show c
    show (JIndex i c) = "[" ++ show i ++ "]" ++ show c

  instance showJsonPrim :: Show JsonPrim where
    show p = runJsonPrim p show show show show

  instance eqJCursor :: Eq JCursor where
    (==) JCursorTop JCursorTop = true
    (==) (JField i1 c1) (JField i2 c2) = i1 == i2 && c1 == c2
    (==) (JIndex i1 c1) (JIndex i2 c2) = i1 == i2 && c1 == c2
    (==) _ _ = false

    (/=) a b = not (a == b)

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
    (<>) a JCursorTop = a
    (<>) JCursorTop b = b
    (<>) (JField i a) b = JField i (a <> b)
    (<>) (JIndex i a) b = JIndex i (a <> b)

  instance monoidJCursor :: Monoid JCursor where
    mempty = JCursorTop