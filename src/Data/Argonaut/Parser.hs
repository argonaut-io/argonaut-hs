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
    , parseByteString
    , ParseResult(..)
) where

import Data.List
import Data.Bits
import Data.Word
import Data.Maybe
import Data.Monoid
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as BU
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
import Data.Argonaut.Templates()
import Debug.Trace

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
  , ("upperE", 69)
  , ("upperF", 70)
  , ("lowerA", 97)
  , ("lowerB", 98)
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

lookupEscapeCharMapping :: Word8 -> Maybe Char
lookupEscapeCharMapping char | char == lowerRChar         = Just '\r'
lookupEscapeCharMapping char | char == lowerNChar         = Just '\n'
lookupEscapeCharMapping char | char == lowerTChar         = Just '\t'
lookupEscapeCharMapping char | char == lowerBChar         = Just '\b'
lookupEscapeCharMapping char | char == lowerFChar         = Just '\f'
lookupEscapeCharMapping char | char == backSlashChar      = Just '\\'
lookupEscapeCharMapping char | char == forwardSlashChar   = Just '/'
lookupEscapeCharMapping char | char == doubleQuoteChar    = Just '"'
lookupEscapeCharMapping _                                 = Nothing

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

data ParseError = UnexpectedTermination
                | InvalidSuffixContent !T.Text
                | UnexpectedContent !T.Text
                | InvalidNumberText !T.Text
                | InvalidEscapeSequence !T.Text
                | ExpectedToken !T.Text !T.Text
                deriving (Eq, Show, Typeable)

newtype ParserInputByteString = ParserInputByteString {runInputByteString :: B.ByteString} deriving (Eq, Ord, Show)

newtype ParserInputText = ParserInputText {runInputText :: T.Text} deriving (Eq, Ord, Show)

data ParseResult a = ParseFailure !ParseError | ParseSuccess !a deriving (Eq, Show)

instance Functor ParseResult where
  fmap _ (ParseFailure x)  = ParseFailure x
  fmap f (ParseSuccess y)  = ParseSuccess (f y)

instance Monad ParseResult where
  return = ParseSuccess
  ParseFailure l >>= _ = ParseFailure l
  ParseSuccess r >>= k = k r

instance Parser Identity ParseResult ParserInputByteString where
  parseJson (Identity json) = parseByteString $ runInputByteString json

instance Parser Identity ParseResult ParserInputText where
  parseJson (Identity json) = parseText $ runInputText json

excerpt :: B.ByteString -> B.ByteString
excerpt !bytestring = B.take 30 bytestring
{-# INLINE excerpt #-}

unexpectedContent :: B.ByteString -> ParseResult a
unexpectedContent !bytestring = ParseFailure $ UnexpectedContent $ T.pack $ show $ excerpt bytestring
{-# INLINE unexpectedContent #-}

isSkipChar :: Word8 -> Bool
isSkipChar !word = {-# SCC isSkipChar #-} word == spaceChar || word == tabChar || word == carriageReturnChar || word == newLineChar
{-# INLINE isSkipChar #-}

charsAtStart :: [Word8] -> B.ByteString -> Bool
charsAtStart !chars !bytestring = {-# SCC charsAtStart #-}
  if B.null bytestring then False else ((BU.unsafeIndex bytestring 0) `elem` chars)
{-# INLINE charsAtStart #-}

singleCharAtStart :: Word8 -> B.ByteString -> Bool
singleCharAtStart !char = charsAtStart [char]

skipSkipChars :: B.ByteString -> (B.ByteString -> a) -> a
skipSkipChars !bytestring !action = 
  let !newByteString = B.dropWhile isSkipChar bytestring
  in  {-# SCC skipSkipChars #-} action newByteString
{-# INLINE skipSkipChars #-}

validSuffixContent :: B.ByteString -> Bool
validSuffixContent !bytestring = skipSkipChars bytestring B.null
{-# INLINE validSuffixContent #-}

parseByteString :: B.ByteString -> ParseResult Json
parseByteString bytestring =
  let value       = expectValue bytestring
      result      = value >>= (\(remainder, json) -> if validSuffixContent remainder then ParseSuccess json else ParseFailure $ InvalidSuffixContent $ T.pack $ show $ excerpt remainder)
  in  {-# SCC parseByteString #-} result

parseText :: T.Text -> ParseResult Json
parseText text = parseByteString $ TE.encodeUtf8 text

isPrefix :: Json -> B.ByteString -> B.ByteString -> ParseResult (B.ByteString, Json)
isPrefix value possiblePrefix bytestring | B.isPrefixOf possiblePrefix bytestring = ParseSuccess (B.drop (B.length possiblePrefix) bytestring, value)
isPrefix _ _ bytestring                                                           = unexpectedContent bytestring
{-# INLINE isPrefix #-}



-- TODO: Parsing, encoding and decoding is actually just encoding...



expectValue :: B.ByteString -> ParseResult (B.ByteString, Json)
expectValue bytestring        = 
  let !isEmpty                = B.null bytestring
      !unexpectedTermination  = ParseFailure $ UnexpectedTermination
      !word                   = BU.unsafeIndex bytestring 0
      !indexResult            = case () of _
                                            | word == openSquareChar        -> expectArray True (BU.unsafeTail bytestring) []
                                            | word == openCurlyChar         -> expectObject True (BU.unsafeTail bytestring) M.empty
                                            | word == doubleQuoteChar       -> fmap (\(remainder, text) -> (remainder, fromText text)) $ expectStringNoStartBounds (BU.unsafeTail bytestring)
                                            | word == lowerTChar            -> isPrefix jsonTrue trueByteString bytestring
                                            | word == lowerFChar            -> isPrefix jsonFalse falseByteString bytestring
                                            | word == lowerNChar            -> isPrefix jsonNull nullByteString bytestring
                                            | word == spaceChar             -> expectValue (BU.unsafeTail bytestring)
                                            | word == carriageReturnChar    -> expectValue (BU.unsafeTail bytestring)
                                            | word == newLineChar           -> expectValue (BU.unsafeTail bytestring)
                                            | word == tabChar               -> expectValue (BU.unsafeTail bytestring)
                                            | otherwise                     -> expectNumber bytestring
  in  {-# SCC expectValue #-} if (isEmpty) then unexpectedTermination else indexResult

expectArray :: Bool -> B.ByteString -> [Json] -> ParseResult (B.ByteString, Json)
expectArray !first !bytestring !elements = {-# SCC expectArray #-} skipSkipChars bytestring (\bytes ->
                                                                       if singleCharAtStart closeSquareChar bytes
                                                                       then ParseSuccess (BU.unsafeTail bytes, fromArray $ JArray $ V.fromList $ reverse elements) 
                                                                       else do afterSeparator <- if first then ParseSuccess bytes else expectEntrySeparator bytes
                                                                               (afterValue, value) <- expectValue afterSeparator
                                                                               expectArray False afterValue $ {-# SCC expectArray_add #-} (value : elements)
                                                                       )


expectObject :: Bool -> B.ByteString -> M.HashMap JString Json -> ParseResult (B.ByteString, Json)
expectObject !first !bytestring !elements = {-# SCC expectObject #-} skipSkipChars bytestring (\bytes ->
                                                                       if singleCharAtStart closeCurlyChar bytes
                                                                       then ParseSuccess (BU.unsafeTail bytes, fromObject $ JObject elements) 
                                                                       else do afterEntrySeparator <- if first then ParseSuccess bytes else expectEntrySeparator bytes
                                                                               (afterKey, key) <- expectString afterEntrySeparator
                                                                               afterFieldSeparator <- expectFieldSeparator afterKey
                                                                               (afterValue, value) <- expectValue afterFieldSeparator
                                                                               expectObject False afterValue $ {-# SCC expectObject_add #-} (M.insert (JString key) value elements)
                                                                       )

expectString :: B.ByteString -> ParseResult (B.ByteString, T.Text)
expectString !bytestring = {-# SCC expectString #-} do afterOpen <- expectStringBounds bytestring
                                                       expectStringNoStartBounds afterOpen

expectStringNoStartBounds :: B.ByteString -> ParseResult (B.ByteString, T.Text)
expectStringNoStartBounds = collectStringParts NoStringParts
{-# INLINE expectStringNoStartBounds #-}

expectSpacerToken :: Word8 -> T.Text -> B.ByteString -> ParseResult B.ByteString
expectSpacerToken !expectedToken failMessage !bytestring = skipSkipChars bytestring (\bytes ->
  if singleCharAtStart expectedToken bytes
  then ParseSuccess (BU.unsafeTail bytes)
  else ParseFailure (ExpectedToken (T.pack $ show expectedToken) failMessage)
  )
{-# INLINE expectSpacerToken #-}

expectEntrySeparator :: B.ByteString -> ParseResult B.ByteString
expectEntrySeparator = expectSpacerToken commaChar "Expected entry separator."
{-# INLINE expectEntrySeparator #-}

expectStringBounds :: B.ByteString -> ParseResult B.ByteString
expectStringBounds = expectSpacerToken doubleQuoteChar "Expected string bounds."
{-# INLINE expectStringBounds #-}

expectFieldSeparator :: B.ByteString -> ParseResult B.ByteString
expectFieldSeparator = expectSpacerToken colonChar "Expected field separator."
{-# INLINE expectFieldSeparator #-}

data StringParts = StringPartsFromBuilder BSB.Builder | StringPartsFromByteString B.ByteString | NoStringParts

createTextFromStringParts :: StringParts -> T.Text
createTextFromStringParts (StringPartsFromBuilder builder)        = TE.decodeUtf8 $ LB.toStrict $ BSB.toLazyByteString builder
createTextFromStringParts (StringPartsFromByteString bytestring)  = TE.decodeUtf8 $ bytestring
createTextFromStringParts NoStringParts                           = ""
{-# INLINE createTextFromStringParts #-}

appendByteStringToStringParts :: StringParts -> B.ByteString -> StringParts
appendByteStringToStringParts (StringPartsFromBuilder builder) toAppend       = StringPartsFromBuilder (builder `mappend` BSB.byteString toAppend)
appendByteStringToStringParts (StringPartsFromByteString bytestring) toAppend = StringPartsFromBuilder (mempty `mappend` BSB.byteString bytestring `mappend` BSB.byteString toAppend)
appendByteStringToStringParts NoStringParts toAppend                          = StringPartsFromByteString toAppend
{-# INLINE appendByteStringToStringParts #-}

appendCharToStringParts :: StringParts -> Char -> StringParts
appendCharToStringParts (StringPartsFromBuilder builder) toAppend       = StringPartsFromBuilder (builder `mappend` BSB.charUtf8 toAppend)
appendCharToStringParts (StringPartsFromByteString bytestring) toAppend = StringPartsFromBuilder (mempty `mappend` BSB.byteString bytestring `mappend` BSB.charUtf8 toAppend)
appendCharToStringParts NoStringParts toAppend                          = StringPartsFromBuilder (mempty `mappend` BSB.charUtf8 toAppend)
{-# INLINE appendCharToStringParts #-}

invalidEscapeSequence :: B.ByteString -> ParseResult a
invalidEscapeSequence !bytestring = ParseFailure (InvalidEscapeSequence $ T.pack $ show $ excerpt bytestring)
{-# INLINE invalidEscapeSequence #-}

getUnicodeEscapeAtStart :: B.ByteString -> Maybe (ParseResult Int)
getUnicodeEscapeAtStart !bytestring | B.length bytestring < 6                           = Nothing
getUnicodeEscapeAtStart !bytestring | BU.unsafeIndex bytestring 0 /= backSlashChar      = Nothing
getUnicodeEscapeAtStart !bytestring | BU.unsafeIndex bytestring 1 /= lowerUChar         = Nothing
getUnicodeEscapeAtStart !bytestring                                                     =
  let !first                = BU.unsafeIndex bytestring 2
      !second               = BU.unsafeIndex bytestring 3
      !third                = BU.unsafeIndex bytestring 4
      !fourth               = BU.unsafeIndex bytestring 5
      !validHex             = validUnicodeHex first second third fourth
      !escapeSequenceValue  = unicodeEscapeSequenceValue first second third fourth
  in  {-# SCC getUnicodeEscapeAtStart #-} Just (if validHex then ParseSuccess escapeSequenceValue else invalidEscapeSequence bytestring)
{-# INLINE getUnicodeEscapeAtStart #-}

getEscapedCharAtStart :: B.ByteString -> Maybe (ParseResult Char)
getEscapedCharAtStart !bytestring | B.length bytestring < 2                           = Nothing
getEscapedCharAtStart !bytestring | BU.unsafeIndex bytestring 0 /= backSlashChar      = Nothing
getEscapedCharAtStart !bytestring                                                     =
  let !char = BU.unsafeIndex bytestring 1
      !result = case char of _
                              | char == lowerRChar         -> Just $ ParseSuccess '\r'
                              | char == lowerNChar         -> Just $ ParseSuccess '\n'
                              | char == lowerTChar         -> Just $ ParseSuccess '\t'
                              | char == lowerBChar         -> Just $ ParseSuccess '\b'
                              | char == lowerFChar         -> Just $ ParseSuccess '\f'
                              | char == backSlashChar      -> Just $ ParseSuccess '\\'
                              | char == forwardSlashChar   -> Just $ ParseSuccess '/'
                              | char == doubleQuoteChar    -> Just $ ParseSuccess '"'
                              | otherwise                  -> Just $ invalidEscapeSequence bytestring
  in  result

collectStringParts :: StringParts -> B.ByteString -> ParseResult (B.ByteString, T.Text)
collectStringParts _      !bytestring | B.null bytestring                               = ParseFailure $ UnexpectedTermination
collectStringParts !parts !bytestring | BU.unsafeIndex bytestring 0 == doubleQuoteChar  = ParseSuccess (BU.unsafeTail bytestring, createTextFromStringParts parts)
collectStringParts !parts !bytestring | BU.unsafeIndex bytestring 0 == backSlashChar    =
  {-# SCC collectStringParts_lead #-} case getUnicodeEscapeAtStart bytestring of 
    Just (ParseFailure failureReason)       -> ParseFailure failureReason
    Just (ParseSuccess escapeSequenceValue) ->
      let !isSurrogateLead = {-# SCC collectStringParts_lead_isSurrogateLead #-} escapeSequenceValue >= 0xD800 && escapeSequenceValue <= 0xDBFF
          escapedChar = {-# SCC collectStringParts_lead_escapedChar #-} toEnum escapeSequenceValue
          surrogateResult = {-# SCC collectStringParts_trail #-} case getUnicodeEscapeAtStart $ B.drop 6 bytestring of
            Just (ParseFailure failureReason)                                                                             -> ParseFailure failureReason
            Just (ParseSuccess surrogateEscapeValue) | surrogateEscapeValue >= 0xDC00 && surrogateEscapeValue <= 0xDFFF   -> 
              let !surrogatePairChar = toEnum ((shiftL 10 escapeSequenceValue - 0xD800) + (surrogateEscapeValue - 0xDC00))
              in  collectStringParts (appendCharToStringParts parts surrogatePairChar) $ B.drop 12 bytestring
            _                                                                                                             -> invalidEscapeSequence bytestring
       in {-# SCC collectStringParts_unicode_result #-} if isSurrogateLead then surrogateResult else {-# SCC collectStringParts_lead_append #-} collectStringParts (appendCharToStringParts parts escapedChar) $ B.drop 6 bytestring
    Nothing                                 -> {-# SCC collectStringParts_getEscapedCharAtStart #-} case getEscapedCharAtStart bytestring of 
                                                  Just (ParseSuccess char)    -> collectStringParts (appendCharToStringParts parts char) $ BU.unsafeDrop 2 bytestring
                                                  Just (ParseFailure failure) -> ParseFailure failure
                                                  Nothing                     -> invalidEscapeSequence bytestring
collectStringParts !parts !bytestring =                                                  
  let (!text, !remainder) = B.span isNormalStringElement bytestring
  in  collectStringParts (appendByteStringToStringParts parts text) remainder

isNormalStringElement :: Word8 -> Bool
isNormalStringElement !word = word /= doubleQuoteChar && word /= backSlashChar
{-# INLINE isNormalStringElement #-}

isHexDigit :: Word8 -> Bool
isHexDigit !word = (word >= lowerAChar && word <= lowerFChar) || (word >= upperAChar && word <= upperFChar) || (word >= zeroChar && word <= nineChar)
{-# INLINE isHexDigit #-}

shiftToHexDigit :: Word8 -> Int
shiftToHexDigit !word | word >= upperAChar && word <= upperFChar  = fromIntegral (word - upperAChar + 10)
shiftToHexDigit !word | word >= lowerAChar && word <= lowerFChar  = fromIntegral (word - lowerAChar + 10)
shiftToHexDigit !word | word >= zeroChar && word <= nineChar      = fromIntegral (word - zeroChar)
shiftToHexDigit _                                                 = error "shiftToHexDigit used incorrectly."
{-# INLINE shiftToHexDigit #-}

validUnicodeHex :: Word8 -> Word8 -> Word8 -> Word8 -> Bool
validUnicodeHex !first !second !third !fourth = isHexDigit first && isHexDigit second && isHexDigit third && isHexDigit fourth
{-# INLINE validUnicodeHex #-}

unicodeEscapeSequenceValue :: Word8 -> Word8 -> Word8 -> Word8 -> Int
unicodeEscapeSequenceValue !first !second !third !fourth =
    let !firstValue    = (shiftToHexDigit first) `shiftL` 12
        !secondValue   = (shiftToHexDigit second) `shiftL` 8
        !thirdValue    = (shiftToHexDigit third) `shiftL` 4
        !fourthValue   = shiftToHexDigit fourth
    in  {-# SCC unicodeEscapeSequenceValue #-} firstValue .|. secondValue .|. thirdValue .|. fourthValue
{-# INLINE unicodeEscapeSequenceValue #-}

wordNumberToInteger :: Word8 -> Integer
wordNumberToInteger !word = fromIntegral $ word - 48
{-# INLINE wordNumberToInteger #-}

wordIsNumber :: Word8 -> Bool
wordIsNumber !word = word >= zeroChar && word <= nineChar
{-# INLINE wordIsNumber #-}

wordIsHyphen :: Word8 -> Bool
wordIsHyphen !word = word == hyphenChar
{-# INLINE wordIsHyphen #-}

wordIsHyphenOrPlus :: Word8 -> Bool
wordIsHyphenOrPlus !word = word == hyphenChar || word == plusChar
{-# INLINE wordIsHyphenOrPlus #-}

wordIsFullStop :: Word8 -> Bool
wordIsFullStop !word = word == fullStopChar
{-# INLINE wordIsFullStop #-}

wordIsE :: Word8 -> Bool
wordIsE !word = word == lowerEChar || word == upperEChar
{-# INLINE wordIsE #-}

collectSign :: Bool -> B.ByteString -> Maybe (Bool, B.ByteString)
collectSign !plusAllowed !bytestring =
  let (!signChars, !remainder) = B.span (if plusAllowed then wordIsHyphenOrPlus else wordIsHyphen) bytestring
      !signCharsLength         = B.length signChars
  in  {-# SCC collectNumber #-} case () of _
                                            | signCharsLength == 1 && not plusAllowed -> Just (False, remainder)
                                            | signCharsLength == 1 && plusAllowed     -> Just (not (B.any wordIsHyphen signChars), remainder)
                                            | signCharsLength == 0                    -> Just (True, remainder)
                                            | otherwise                               -> Nothing

collectNumber :: B.ByteString -> Maybe (Integer, B.ByteString)
collectNumber !bytestring =
  let (!numberWords, !remainder)  = B.span wordIsNumber bytestring
      !numberResult               = B.foldl' (\number -> \word -> (number * 10) + (wordNumberToInteger word)) 0 numberWords
  in  {-# SCC collectNumber #-} case (B.length numberWords) of
                                                              0 -> Nothing
                                                              _ -> Just (numberResult, remainder)

collectFractionalPrefix :: B.ByteString -> Maybe (Bool, B.ByteString)
collectFractionalPrefix !bytestring =
  let (!fractionalPrefix, !remainder) = B.span wordIsFullStop bytestring
  in  {-# SCC collectFractionalPrefix #-} case (B.length fractionalPrefix) of
                                                                              0 -> Just (False, remainder)
                                                                              1 -> Just (True, remainder)
                                                                              _ -> Nothing

collectExponentialPrefix :: B.ByteString -> Maybe (Bool, B.ByteString)
collectExponentialPrefix !bytestring =
  let (!exponentialPrefix, !remainder) = B.span wordIsE bytestring
  in  {-# SCC collectExponentialPrefix #-} case (B.length exponentialPrefix) of 
                                                                                0 -> Just (False, remainder)
                                                                                1 -> Just (True, remainder)
                                                                                _ -> Nothing

collectSignedNumber :: Bool -> B.ByteString -> Maybe (Integer, Bool, B.ByteString)
collectSignedNumber !plusAllowed !bytestring = do (positive, postSign)  <- collectSign plusAllowed bytestring
                                                  (number, postNumber)  <- collectNumber postSign
                                                  {-# SCC collectSignedNumber #-} return ((if positive then number else number * (-1)), positive, postNumber)

expectNumber :: B.ByteString -> ParseResult (B.ByteString, Json)
expectNumber !bytestring = 
  let parsedNumber = {-# SCC expectNumber #-}  do (!mantissa, !positive, !postMantissa)   <-  collectSignedNumber False bytestring
                                                  (!isFractional, !postPoint)             <-  collectFractionalPrefix postMantissa
                                                  (!fractional, !postFractional)          <-  if isFractional then collectNumber postPoint else return (0, postPoint)
                                                  let !fractionalDigits                   =   B.length postPoint - B.length postFractional
                                                  (!isExponential, !postE)                <-  collectExponentialPrefix postFractional
                                                  (!exponential, _, !postExponential)     <-  if isExponential then collectSignedNumber True postE else return (0, True, postE)
                                                  let !mantissaAsScientific               =   fromIntegral mantissa
                                                  let !fractionalAsScientific             =   {-# SCC expectNumber_asScientific #-} (fromIntegral (if positive then fractional else (-fractional))) * (10 ^^ (-fractionalDigits))
                                                  let !mantissaPlusFractional             =   {-# SCC expectNumber_plusFractional #-} mantissaAsScientific + fractionalAsScientific
                                                  let !numberResult                       =   {-# SCC expectNumber_numberResult #-} mantissaPlusFractional * (10 ^^ exponential)
                                                  !numberJson                             <-  fromScientific $ numberResult
                                                  return                              (postExponential, numberJson)
  in  maybe (ParseFailure (InvalidNumberText $ T.pack $ show $ excerpt bytestring)) (\(newByteString, number) -> ParseSuccess (newByteString, number)) parsedNumber