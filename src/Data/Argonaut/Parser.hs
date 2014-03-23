{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Argonaut.Parser
  (
      Parser(..)
    , parse
    , parseText
    , TextErrorParseResult(..)
) where

import Data.List
import Data.Bits
import Data.Word
import Data.Monoid
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Builder as BSB
import Data.Argonaut
import Data.Argonaut.Templates
import Control.Monad.Identity
import Text.Read
import Data.Typeable(Typeable)
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Argonaut.Templates

$(buildWord8s [
    ("forwardSlash", 47)
  , ("backSlash", 92)
  , ("closeCurly", 125)
  , ("closeSquare", 93)
  , ("comma", 44)
  , ("doubleQuote", 34)
  , ("openCurly", 123)
  , ("openSquare", 91)
  , ("plus", 43)
  , ("hyphen", 45)
  , ("fullStop", 46)
  , ("zero", 48)
  , ("one", 49)
  , ("two", 50)
  , ("three", 51)
  , ("four", 52)
  , ("five", 53)
  , ("six", 54)
  , ("seven", 55)
  , ("eight", 56)
  , ("nine", 57)
  , ("upperA", 65)
  , ("upperB", 66)
  , ("upperC", 67)
  , ("upperD", 68)
  , ("upperE", 69)
  , ("upperF", 70)
  , ("lowerA", 97)
  , ("lowerB", 98)
  , ("lowerC", 99)
  , ("lowerD", 100)
  , ("lowerE", 101)
  , ("lowerF", 102)
  , ("lowerN", 110)
  , ("lowerR", 114)
  , ("lowerT", 116)
  , ("lowerU", 117)
  , ("tab", 9)
  , ("space", 32)
  , ("carriageReturn", 13)
  , ("newLine", 10)
  , ("colon", 58)])

skipChars :: [Word8]
skipChars = [spaceChar, tabChar, carriageReturnChar, newLineChar]

escapeCharMappings :: M.HashMap Word8 BSB.Builder
escapeCharMappings = 
    let mappings = [(lowerRChar, '\r'), (lowerNChar, '\n'), (lowerTChar, '\t'), (lowerBChar, '\b'), (lowerFChar, '\f'), (backSlashChar, '\\'), (forwardSlashChar, '/'), (doubleQuoteChar, '"')]
    in  foldl' (\mappingsMap -> \(char, mappedChar) -> M.insert char (BSB.charUtf8 mappedChar) mappingsMap) M.empty mappings

lookupEscapeCharMapping :: Word8 -> Maybe BSB.Builder
lookupEscapeCharMapping char = M.lookup char escapeCharMappings

trueByteString :: B.ByteString
trueByteString = TE.encodeUtf8 "true"

falseByteString :: B.ByteString
falseByteString = TE.encodeUtf8 "false"

nullByteString :: B.ByteString
nullByteString = TE.encodeUtf8 "null"

class Parser m n a | m a -> n where
  parseJson :: m a -> n Json

parse :: Parser m n a => m a -> n Json
parse = parseJson

data ParseError a = UnexpectedTermination !Int
                  | InvalidSuffixContent !a
                  | UnexpectedContent !Int
                  | InvalidNumberText !Int
                  | InvalidEscapeSequence !a
                  | ExpectedToken !a !a
                  deriving (Eq, Show, Typeable)

newtype ParserInputString = ParserInputString String deriving (Eq, Ord, Show)

newtype ParserInputText = ParserInputText T.Text deriving (Eq, Ord, Show)

runInputString :: ParserInputString -> String
runInputString (ParserInputString value) = value

runInputText :: ParserInputText -> T.Text
runInputText (ParserInputText value) = value

data StringErrorParseResult a = StringErrorParseFailure !(ParseError String) | StringErrorParseSuccess !a deriving (Eq, Show)

data TextErrorParseResult a = TextErrorParseFailure !(ParseError T.Text) | TextErrorParseSuccess !a deriving (Eq, Show)

instance Functor StringErrorParseResult where
  fmap _ (StringErrorParseFailure x) = StringErrorParseFailure x
  fmap f (StringErrorParseSuccess y) = StringErrorParseSuccess (f y)

instance Functor TextErrorParseResult where
  fmap _ (TextErrorParseFailure x) = TextErrorParseFailure x
  fmap f (TextErrorParseSuccess y) = TextErrorParseSuccess (f y)

instance Monad StringErrorParseResult where
  return = StringErrorParseSuccess
  StringErrorParseFailure l >>= _ = StringErrorParseFailure l
  StringErrorParseSuccess r >>= k = k r

instance Monad TextErrorParseResult where
  return = TextErrorParseSuccess
  TextErrorParseFailure l >>= _ = TextErrorParseFailure l
  TextErrorParseSuccess r >>= k = k r

{-
Reinstate later as a layer over parseText?
instance Parser Identity StringErrorParseResult ParserInputString where
  parseJson (Identity json) = parseString $ runInputString json
-}

instance Parser Identity TextErrorParseResult ParserInputText where
  parseJson (Identity json) = parseText $ runInputText json

ifValidIndex :: (Word8 -> TextErrorParseResult a) -> Int -> B.ByteString -> TextErrorParseResult a 
ifValidIndex wordLookup index bytestring = 
  let parseError  = TextErrorParseFailure $ UnexpectedTermination index
      success     = wordLookup $ B.index bytestring index
      result      = if index < B.length bytestring then success else parseError
  in  result

excerpt :: B.ByteString -> Int -> T.Text
excerpt bytestring index = T.take 30 $ T.drop index $ TE.decodeUtf8 bytestring

unexpectedContent :: B.ByteString -> Int -> TextErrorParseResult a
unexpectedContent bytestring index = TextErrorParseFailure $ UnexpectedContent index

singleCharAtIndex :: Word8 -> B.ByteString -> Int -> Bool
singleCharAtIndex char bytestring index = 
  let result = charAtIndex [char] bytestring index
  in  result

charAtIndex :: [Word8] -> B.ByteString -> Int -> Bool
charAtIndex chars bytestring index = case (ifValidIndex (fmap TextErrorParseSuccess (`elem` chars)) index bytestring) of
                                          TextErrorParseSuccess result  -> result
                                          _                             -> False

skipSkipChars :: B.ByteString -> Int -> (B.ByteString -> Int -> a) -> a
skipSkipChars bytestring index action = 
  let toSkip = B.length $ B.takeWhile (`elem` skipChars) $ B.drop index bytestring
  in  action bytestring (index + toSkip)

validSuffixContent :: B.ByteString -> Int -> Bool
validSuffixContent (B.length -> stringLength) index | index == stringLength     = True
validSuffixContent bytestring index                                             = charAtIndex [spaceChar, carriageReturnChar, newLineChar, tabChar] bytestring index && validSuffixContent bytestring (index + 1)

{-
checkSuffix :: (String, Json) -> StringErrorParseResult Json
checkSuffix (suffix, json) = if validSuffixContent suffix then StringErrorParseSuccess json else StringErrorParseFailure (InvalidSuffixContent suffix)
-}

parseText :: T.Text -> TextErrorParseResult Json
parseText text =
  let bytestring  = TE.encodeUtf8 text
      value       = expectValue bytestring 0
      result      = value >>= (\(index, json) -> if (B.length bytestring == index) then TextErrorParseSuccess json else TextErrorParseFailure $ InvalidSuffixContent $ excerpt bytestring index)
  in  result

isPrefix :: Json -> B.ByteString -> B.ByteString -> Int -> TextErrorParseResult (Int, Json)
isPrefix value possiblePrefix bytestring index | B.isPrefixOf possiblePrefix $ B.drop index bytestring  = TextErrorParseSuccess ((index + (B.length possiblePrefix)), value)
isPrefix _ _ bytestring index                                                                           = unexpectedContent bytestring index

expectValue :: B.ByteString -> Int -> TextErrorParseResult (Int, Json)
expectValue bytestring index = 
  let validIndex            = index < B.length bytestring
      unexpectedTermination = TextErrorParseFailure $ UnexpectedTermination index
      word                  = B.index bytestring index
      indexResult           = case () of _
                                            | word == openSquareChar        -> expectArray True bytestring (index + 1) V.empty
                                            | word == openCurlyChar         -> expectObject True bytestring (index + 1) M.empty
                                            | word == doubleQuoteChar       -> fmap (\(ind, text) -> (ind, fromText text)) $ expectStringNoStartBounds bytestring (index + 1)
                                            | word == lowerTChar            -> isPrefix jsonTrue trueByteString bytestring index
                                            | word == lowerFChar            -> isPrefix jsonFalse falseByteString bytestring index
                                            | word == lowerNChar            -> isPrefix jsonNull nullByteString bytestring index
                                            | word == spaceChar             -> expectValue bytestring (index + 1)
                                            | word == carriageReturnChar    -> expectValue bytestring (index + 1)
                                            | word == newLineChar           -> expectValue bytestring (index + 1)
                                            | word == tabChar               -> expectValue bytestring (index + 1)
                                            | otherwise                     -> expectNumber bytestring index
  in  if (validIndex) then indexResult else unexpectedTermination

expectArray :: Bool -> B.ByteString -> Int -> V.Vector Json -> TextErrorParseResult (Int, Json)
expectArray first bytestring index elements = skipSkipChars bytestring index (\bytes -> \ind -> 
                                                                       if singleCharAtIndex closeSquareChar bytes ind 
                                                                       then TextErrorParseSuccess (ind + 1, fromArray $ JArray elements) 
                                                                       else do afterSeparator <- if first then TextErrorParseSuccess ind else expectEntrySeparator bytes ind
                                                                               (afterValue, value) <- expectValue bytestring afterSeparator
                                                                               expectArray False bytes afterValue (V.snoc elements value)
                                                                       )


expectObject :: Bool -> B.ByteString -> Int -> M.HashMap JString Json -> TextErrorParseResult (Int, Json)
expectObject first bytestring index elements = skipSkipChars bytestring index (\bytes -> \ind -> 
                                                                       if singleCharAtIndex closeCurlyChar bytes ind 
                                                                       then TextErrorParseSuccess (ind + 1, fromObject $ JObject elements) 
                                                                       else do afterEntrySeparator <- if first then TextErrorParseSuccess index else expectEntrySeparator bytestring index
                                                                               (afterKey, key) <- expectString bytestring afterEntrySeparator
                                                                               afterFieldSeparator <- expectFieldSeparator bytestring afterKey
                                                                               (afterValue, value) <- expectValue bytestring afterFieldSeparator
                                                                               expectObject False bytes afterValue (M.insert (JString key) value elements)
                                                                       )

expectString :: B.ByteString -> Int -> TextErrorParseResult (Int, T.Text)
expectString bytestring index = do afterOpen <- expectStringBounds bytestring index
                                   expectStringNoStartBounds bytestring afterOpen

expectStringNoStartBounds :: B.ByteString -> Int -> TextErrorParseResult (Int, T.Text)
expectStringNoStartBounds = collectStringParts (BSB.byteString B.empty)

expectSpacerToken :: Word8 -> T.Text -> B.ByteString -> Int -> TextErrorParseResult Int
expectSpacerToken expectedToken failMessage bytestring index = if singleCharAtIndex expectedToken bytestring index then TextErrorParseSuccess (index + 1) else TextErrorParseFailure (ExpectedToken (T.pack $ show expectedToken) failMessage)

expectEntrySeparator :: B.ByteString -> Int -> TextErrorParseResult Int
expectEntrySeparator = expectSpacerToken commaChar "Expected entry separator."

expectStringBounds :: B.ByteString -> Int -> TextErrorParseResult Int
expectStringBounds = expectSpacerToken doubleQuoteChar "Expected string bounds."

expectFieldSeparator :: B.ByteString -> Int -> TextErrorParseResult Int
expectFieldSeparator = expectSpacerToken colonChar "Expected field separator."

collectStringParts :: BSB.Builder -> B.ByteString -> Int -> TextErrorParseResult (Int, T.Text)
collectStringParts _     bytestring index | B.length bytestring <= index          = TextErrorParseFailure $ UnexpectedTermination index
collectStringParts parts bytestring index | B.index bytestring index == doubleQuoteChar  = TextErrorParseSuccess (index + 1, TE.decodeUtf8 $ LB.toStrict $ BSB.toLazyByteString parts)
collectStringParts parts bytestring index | B.index bytestring index == backSlashChar   = case (B.unpack $ B.take 5 $ B.drop (index + 1) bytestring) of 
                                                                                            escapeSeq@(possibleUChar : first : second : third : fourth : []) | possibleUChar == lowerUChar ->
                                                                                               let validHex = validUnicodeHex first second third fourth
                                                                                                   invalidEscapeSequence = TextErrorParseFailure (InvalidEscapeSequence $ T.pack $ show escapeSeq)
                                                                                                   escapeSequenceValue = unicodeEscapeSequenceValue first second third fourth
                                                                                                   isSurrogateLead = escapeSequenceValue >= 0xD800 && escapeSequenceValue <= 0xDBFF
                                                                                                   escapedChar = toEnum escapeSequenceValue
                                                                                                   surrogateResult = case (B.unpack $ B.take 6 $ B.drop (index + 6) bytestring) of
                                                                                                      possibleBackSlash : possibleU : trailFirst : trailSecond : trailThird : trailFourth : [] | possibleBackSlash == backSlashChar && possibleU == lowerUChar ->
                                                                                                        let validTrailHex = validUnicodeHex trailFirst trailSecond trailThird trailFourth
                                                                                                            trailEscapeSequenceValue = unicodeEscapeSequenceValue trailFirst trailSecond trailThird trailFourth
                                                                                                            isSurrogateTrail = trailEscapeSequenceValue >= 0xDC00 && trailEscapeSequenceValue <= 0xDFFF
                                                                                                            surrogatePairChar = toEnum ((shiftL 10 escapeSequenceValue - 0xD800) + (trailEscapeSequenceValue - 0xDC00))
                                                                                                        in if validTrailHex && isSurrogateTrail then (collectStringParts (parts `mappend` BSB.charUtf8 surrogatePairChar) bytestring (index + 12)) else invalidEscapeSequence
                                                                                                      _ -> invalidEscapeSequence
                                                                                                   validResult = if isSurrogateLead then surrogateResult else collectStringParts (parts `mappend` BSB.charUtf8 escapedChar) bytestring (index + 6)
                                                                                               in if validHex then validResult else invalidEscapeSequence
                                                                                            (lookupEscapeCharMapping -> Just char) : _ -> collectStringParts (parts `mappend` char) bytestring (index + 2)
                                                                                            invalidSeq -> TextErrorParseFailure (InvalidEscapeSequence $ T.pack $ show invalidSeq)
collectStringParts parts bytestring index                                               =
                                                                                        let text = B.takeWhile isNormalStringElement $ B.drop index bytestring
                                                                                        in  collectStringParts (parts `mappend` BSB.byteString text) bytestring (index + B.length text)


isNormalStringElement :: Word8 -> Bool
isNormalStringElement word = word /= doubleQuoteChar && word /= backSlashChar

isHexDigit :: Word8 -> Bool
isHexDigit word = 
  let result = (word >= lowerAChar && word <= lowerFChar) || (word >= upperAChar && word <= upperFChar) || (word >= zeroChar && word <= nineChar)
  in  result

shiftToHexDigit :: Word8 -> Int
shiftToHexDigit word | word >= upperAChar && word <= upperFChar   = fromIntegral (word - upperAChar + 10)
shiftToHexDigit word | word >= lowerAChar && word <= lowerFChar   = fromIntegral (word - lowerAChar + 10)
shiftToHexDigit word | word >= zeroChar && word <= nineChar       = fromIntegral (word - zeroChar)
shiftToHexDigit _                                                 = error "shiftToHexDigit used incorrectly."

validUnicodeHex :: Word8 -> Word8 -> Word8 -> Word8 -> Bool
validUnicodeHex !first !second !third !fourth = isHexDigit first && isHexDigit second && isHexDigit third && isHexDigit fourth

unicodeEscapeSequenceValue :: Word8 -> Word8 -> Word8 -> Word8 -> Int
unicodeEscapeSequenceValue first second third fourth =
    let firstValue    = (shiftToHexDigit first) `shiftL` 12
        secondValue   = (shiftToHexDigit second) `shiftL` 8
        thirdValue    = (shiftToHexDigit third) `shiftL` 4
        fourthValue   = shiftToHexDigit fourth
    in  firstValue .|. secondValue .|. thirdValue .|. fourthValue

isNumberChar :: Word8 -> Bool
isNumberChar char = (char >= zeroChar && char <= nineChar) || char == plusChar || char == hyphenChar || char == lowerEChar || char == upperEChar || char == fullStopChar

expectNumber :: B.ByteString -> Int -> TextErrorParseResult (Int, Json)
expectNumber bytestring index =
                  let numberPrefix  = B.takeWhile isNumberChar $ B.drop index bytestring
                      prefixLength  = B.length numberPrefix
                      parsedNumber  = readMaybe $ T.unpack $ TE.decodeUtf8 numberPrefix
                      number        = parsedNumber >>= fromDouble
                  in  maybe (TextErrorParseFailure (InvalidNumberText index)) (\n -> TextErrorParseSuccess (index + prefixLength, n)) number
