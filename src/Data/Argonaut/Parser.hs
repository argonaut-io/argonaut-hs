{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# lANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ViewPatterns #-}

module Data.Argonaut.Parser
  (
      Parser(..)
    , parse
    , parseString
    , ParserInputString
    , StringErrorParseResult(..)
) where

import Data.Bits
import Data.Word
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Builder as BSB
import Data.Char
import Data.Argonaut
import Control.Monad.Identity
import Text.Read
import Data.Typeable(Typeable)
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE



backSlashChar :: Word8
backSlashChar = 92

closeCurlyChar :: Word8 
closeCurlyChar = 125

closeSquareChar :: Word8 
closeSquareChar = 93

commaChar :: Word8 
commaChar = 44

doubleQuoteChar :: Word8 
doubleQuoteChar = 34

openCurlyChar :: Word8 
openCurlyChar = 123

openSquareChar :: Word8 
openSquareChar = 91

zeroChar :: Word8 
zeroChar = 48

nineChar :: Word8 
nineChar = 57

upperAChar :: Word8 
upperAChar = 65

upperFChar :: Word8 
upperFChar = 70

lowerAChar :: Word8 
lowerAChar = 97

lowerBChar :: Word8
lowerBChar = 98

lowerFChar :: Word8 
lowerFChar = 102

lowerNChar :: Word8
lowerNChar = 110

lowerRChar :: Word8
lowerRChar = 114

lowerTChar :: Word8
lowerTChar = 116

lowerUChar :: Word8
lowerUChar = 117

rChar :: Word8 
rChar = 114

tChar :: Word8
tChar = 116

nChar :: Word8 
nChar = 110

tabChar :: Word8 
tabChar = 116

spaceChar :: Word8 
spaceChar = 32

carriageReturnChar :: Word8 
carriageReturnChar = 13

newLineChar :: Word8 
newLineChar = 10

speechMarkChar :: Word8
speechMarkChar = 34

colonChar :: Word8
colonChar = 58

skipChars :: [Word8]
skipChars = [spaceChar, tabChar, carriageReturnChar, newLineChar]

escapeCharMappings :: M.HashMap Word8 BSB.Builder
escapeCharMappings = 
    let mappings = [(lowerRChar, '\r'), (lowerNChar, '\n'), (lowerTChar, '\t'), (lowerBChar, '\b'), (lowerFChar, '\f'), (backSlashChar, '\\'), (forwardSlashChar, '/'), (speechMarkChar, '"')]
    in  foldL' (\map -> \(char, mappedChar) -> M.insert char (BSB.charUtf8 mappedChar) map) M.empty mappings

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

data ParseError a = UnexpectedTermination
                  | InvalidSuffixContent !a
                  | UnexpectedContent !a
                  | InvalidNumberText !a
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

ifValidIndex :: (Word8 -> a) -> Int -> B.ByteString -> TextErrorParseResult a 
ifValidIndex lookup index bytestring = 
  let error = TextErrorParseFailure UnexpectedTermination
      success = TextErrorParseSuccess $ lookup $ B.index bytestring index
      result = if index < B.length bytestring then success else error
  in result

excerpt :: B.ByteString -> Int -> B.ByteString
excerpt bytestring index = TE.encodeUtf8 . T.take 30 . T.drop index . TE.decodeUtf8

unexpectedContent :: B.ByteString -> Int -> TextErrorParseResult 
unexpectedContent bytestring index = TextErrorParseFailure UnexpectedContent . excerpt bytestring index

singleCharAtIndex :: Word8 -> B.ByteString -> Int -> Bool
singleCharAtIndex char bytestring index = charAtIndex [char] bytestring index

charAtIndex :: [Word8] -> B.ByteString -> Int -> Bool
charAtIndex chars (ifValidIndex (`elem` chars) index -> TextErrorParseSuccess _) index = True
charAtIndex _ _ _ = False

skipSkipChars :: B.ByteString -> Int -> (B.ByteString -> Int -> a) -> a
skipSkipChars bytestring index action = if charAtIndex skipChars bytestring index then skipSkipChars bytestring (index + 1) action else action bytestring index

validSuffixContent :: B.ByteString -> Int -> Bool
validSuffixContent index (B.length -> stringLength) | index == stringLength = True
validSuffixContent index bytestring = charAtIndex [spaceChar, carriageReturnChar, newLineChar, tabChar] bytestring index && validSuffixContent (index + 1) bytestring
validSuffixContent _ _ = False

checkSuffix :: (String, Json) -> StringErrorParseResult Json
checkSuffix (suffix, json) = if validSuffixContent suffix then StringErrorParseSuccess json else StringErrorParseFailure (InvalidSuffixContent suffix)

parseText :: T.Text -> TextErrorParseResult Json
parseText text =
  let bytestring = TE.encodeUtf8 text
      value = expectValue bytestring 0
      result = value >>= (\(index, json) -> if (B.length bytestring == index) then TextErrorParseSuccess json else TextErrorParseFailure InvalidSuffixContent excerpt bytestring index)
  in result

isPrefix :: B.ByteString -> Int -> Json -> B.ByteString -> TextErrorParseResult (Int, Json)
isPrefix possiblePrefix index value bytestring = if (B.isPrefixOf possiblePrefix $ B.drop index bytestring) then TextErrorParseSuccess ((index + B.length possiblePrefix) value) else TextErrorParseFailure unexpectedContent bytestring

expectValueCond :: B.ByteString -> Int -> Word8 -> TextErrorParseResult (Int, Json)
expectValueCond bytestring index openSquareChar = expectArray bytestring (index + 1) True V.empty
expectValueCond bytestring index openCurlyChar = expectObject bytestring (index + 1) True M.empty
expectValueCond bytestring index doubleQuoteChar = expectStringNoStartBounds bytestring (index + 1)
expectValueCond bytestring index lowerTChar = isPrefix trueByteString index jsonTrue bytestring
expectValueCond bytestring index lowerFChar = isPrefix falseByteString index jsonFalse bytestring
expectValueCond bytestring index lowerNChar = isPrefix nullByteString index jsonNull bytestring
expectValueCond bytestring index spaceChar = ifValidIndex (expectValueCond bytestring (index + 1)) (index + 1) bytestring
expectValueCond bytestring index carriageReturnChar = ifValidIndex (expectValueCond bytestring (index + 1)) (index + 1) bytestring
expectValueCond bytestring index newLineChar = ifValidIndex (expectValueCond bytestring (index + 1)) (index + 1) bytestring
expectValueCond bytestring index tabChar = ifValidIndex (expectValueCond bytestring (index + 1)) (index + 1) bytestring
expectValueCond bytestring index _  = expectNumber bytestring index

expectValue :: B.ByteString -> Int -> TextErrorParseResult (Int, Json)
expectValue bytestring index = (expectValueCond bytestring index) bytestring index

expectSpacerToken :: B.ByteString -> Int -> TextErrorParseResult Int
expectSpacerToken bytestring index = if charAtIndex skipChars bytestring index 

expectArray :: Bool -> B.ByteString -> Int -> V.Vector Json -> TextErrorParseResult (Int, Json)
expectArray first bytestring index elements = skipSkipChars bytestring index (\bytes -> \ind -> 
                                                                       if singleCharAtIndex closeSquareChar bytes ind 
                                                                       then TextErrorParseSuccess (ind + 1, JArray elements) 
                                                                       else do afterSeparator <- if first then TextErrorParseSuccess ind else expectEntrySeparator bytes ind
                                                                               (afterValue, value) <- expectValue afterSeparator
                                                                               expectArray False bytes afterValue (V.snoc elements value)


expectObject :: Bool -> B.ByteString -> Int -> M.HashMap JString Json -> TextErrorParseResult (Int, Json)
expectObject first bytestring index field = skipSkipChars bytestring index (\bytes -> \ind -> 
                                                                       if singleCharAtIndex closeSquareChar bytes ind 
                                                                       then TextErrorParseSuccess (ind + 1, JArray elements) 
                                                                       else do afterEntrySeparator <- if first then TextErrorParseSuccess text else expectEntrySeparator text
                                                                               (afterKey, key) <- expectString afterEntrySeparator
                                                                               afterFieldSeparator <- expectFieldSeparator afterKey
                                                                               (afterValue, value) <- expectValue afterFieldSeparator
                                                                               expectObject False bytes afterValue (M.insert (JString key) value fields)

expectString :: B.ByteString -> Int -> TextErrorParseResult (Int, T.Text)
expectString bytestring index = do afterOpen <- expectStringBounds bytestring index
                                   expectStringNoStartBounds afterOpen

expectStringNoStartBounds :: B.ByteString -> Int -> TextErrorParseResult (Int, T.Text)
expectStringNoStartBounds bytestring index  = do (remainder, textOfString) <- collectStringParts text index (BSB.byteString B.empty)
                                                 return (remainder, reverse textOfString)

expectSpacerToken :: B.ByteString -> Int -> Word8 -> T.Text -> TextErrorParseResult Int
expectSpacerToken bytestring index expectedToken failMessage = if singleCharAtIndex expectedToken bytestring index then TextErrorParseSuccess (index + 1) else TextErrorParseFailure (ExpectedToken (show expectedToken) failMessage)

expectEntrySeparator :: B.ByteString -> Int -> TextErrorParseResult String
expectEntrySeparator bytestring index = expectSpacerToken bytestring index commaChar "Expected entry separator."

expectStringBounds :: B.ByteString -> Int -> TextErrorParseResult String
expectStringBounds bytestring index = expectSpacerToken bytestring index speechMarkChar "Expected string bounds."

expectFieldSeparator :: B.ByteString -> Int -> TextErrorParseResult String
expectFieldSeparator bytestring index = expectSpacerToken bytestring index colonChar "Expected field separator."

collectStringParts :: B.ByteString -> Int -> BSB.Builder -> TextErrorParseResult (Int, T.Text)
collectStringParts bytestring index _ | B.length bytestring <= index = TextErrorParseFailure UnexpectedTermination
collectStringParts bytestring index parts | B.index bytestring == speechMarkChar = StringErrorParseSuccess (index + 1, LB.toString $ BSB.toLazyByteString parts)
collectStringParts bytestring index parts | B.index bytestring == backSlashChar = case (B.unpack $ B.take 5 $ B.drop (index + 1) bytestring) of 
                                                                                    lowerUChar : first : second : third : fourth : [] ->
                                                                                       let validHex = validUnicodeHex first second third fourth
                                                                                           invalidEscapeSequence = StringErrorParseFailure (InvalidEscapeSequence ('\\' : 'u' : first : second : third : fourth : remainder))
                                                                                           escapeSequenceValue = unicodeEscapeSequenceValue first second third fourth
                                                                                           isSurrogateLead = escapeSequenceValue >= 0xD800 && escapeSequenceValue <= 0xDBFF
                                                                                           escapedChar = toEnum escapeSequenceValue
                                                                                           surrogateResult = case remainder of
                                                                                              '\\' : 'u' : trailFirst : trailSecond : trailThird : trailFourth : trailRemainder ->
                                                                                                let validTrailHex = validUnicodeHex trailFirst trailSecond trailThird trailFourth
                                                                                                    trailEscapeSequenceValue = unicodeEscapeSequenceValue trailFirst trailSecond trailThird trailFourth
                                                                                                    isSurrogateTrail = trailEscapeSequenceValue >= 0xDC00 && trailEscapeSequenceValue <= 0xDFFF
                                                                                                    surrogatePairChar = toEnum ((shiftL 10 escapeSequenceValue - 0xD800) + (trailEscapeSequenceValue - 0xDC00))
                                                                                                in if validTrailHex && isSurrogateTrail then (collectStringParts trailRemainder (surrogatePairChar : workingString)) else invalidEscapeSequence
                                                                                              _ -> invalidEscapeSequence
                                                                                           validResult = if isSurrogateLead then surrogateResult else collectStringParts remainder (escapedChar : workingString)
                                                                                       in if validHex then validResult else invalidEscapeSequence
                                                                                    (lookupEscapeCharMapping -> Maybe char) : _ -> collectStringParts bytestring (index + 2) (parts `mappend` char)
                                                                                    otherwise -> TextErrorParseFailure (InvalidEscapeSequence ('\\' : remainder))
collectStringParts (char : remainder) workingString = collectStringParts remainder (char : workingString)

validUnicodeHex :: Word8 -> Word8 -> Word8 -> Word8 -> Bool
validUnicodeHex !first !second !third !fourth = isHexDigit first && isHexDigit second && isHexDigit third && isHexDigit fourth

unicodeEscapeSequenceValue :: Word8 -> Word8 -> Word8 -> Word8 -> Int
unicodeEscapeSequenceValue first second third fourth =
    let firstValue = (digitToInt first) `shiftL` 12
        secondValue = (digitToInt second) `shiftL` 8
        thirdValue = (digitToInt third) `shiftL` 4
        fourthValue = digitToInt fourth
    in firstValue .|. secondValue .|. thirdValue .|. fourthValue

isNumberChar :: Word8 -> Bool
isNumberChar char = (char >= '0' && char <= '9') || char == '+' || char == '-' || char == 'e' || char == 'E' || char == '.'

expectNumber :: String -> StringErrorParseResult (String, Json)
expectNumber text =
                  let (numberPrefix, remainder) = span isNumberChar text
                      parsedNumber = readMaybe numberPrefix
                      number = parsedNumber >>= fromDouble
                  in maybe (StringErrorParseFailure (InvalidNumberText numberPrefix)) (\n -> StringErrorParseSuccess (remainder, n)) number
