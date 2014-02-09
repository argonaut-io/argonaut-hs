{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ViewPatterns #-}

module Data.Argonaut.Parser
  (
      Parser(..)
    , parse
    , parseString
    , ParserInputString
    , StringErrorParseResult(..)
    , utfParts
) where

import qualified Data.Text as T

import Data.Bits
import Data.Char
import Data.Argonaut
import Control.Monad.Identity
import Text.Read
import Data.Typeable(Typeable)
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as M

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

newtype ParserInputString = ParserInputString T.Text deriving (Eq, Ord, Show)

runInputString :: ParserInputString -> T.Text
runInputString (ParserInputString value) = value

data StringErrorParseResult a = StringErrorParseFailure !(ParseError T.Text) | StringErrorParseSuccess !a deriving (Eq, Show)

instance Functor StringErrorParseResult where
  fmap _ (StringErrorParseFailure x) = StringErrorParseFailure x
  fmap f (StringErrorParseSuccess y) = StringErrorParseSuccess (f y)

instance Monad StringErrorParseResult where
  return = StringErrorParseSuccess
  StringErrorParseFailure l >>= _ = StringErrorParseFailure l
  StringErrorParseSuccess r >>= k = k r

instance Parser Identity StringErrorParseResult ParserInputString where
  parseJson (Identity json) = parseString $ runInputString json

validSuffixContent :: T.Text -> Bool
validSuffixContent (T.stripPrefix " " -> Just remainder) = validSuffixContent remainder
validSuffixContent (T.stripPrefix "\r" -> Just remainder) = validSuffixContent remainder
validSuffixContent (T.stripPrefix "\n" -> Just remainder) = validSuffixContent remainder
validSuffixContent (T.stripPrefix "\t" -> Just remainder) = validSuffixContent remainder
validSuffixContent text = T.null text

checkSuffix :: (T.Text, Json) -> StringErrorParseResult Json
checkSuffix (suffix, json) = if validSuffixContent suffix then StringErrorParseSuccess json else StringErrorParseFailure (InvalidSuffixContent suffix)

parseString :: T.Text -> StringErrorParseResult Json
parseString text = expectValue text >>= checkSuffix

expectValue :: T.Text -> StringErrorParseResult (T.Text, Json)
expectValue "" = StringErrorParseFailure UnexpectedTermination
expectValue (T.stripPrefix "[" -> Just remainder) = expectArray True V.empty remainder
expectValue (T.stripPrefix "{" -> Just remainder) = expectObject True M.empty remainder
expectValue (T.stripPrefix "\"" -> Just remainder) = fmap (\(r, text) -> (r, fromString text)) (expectStringNoStartBounds remainder)
expectValue (T.stripPrefix "true" -> Just remainder) = StringErrorParseSuccess (remainder, jsonTrue)
expectValue (T.stripPrefix "false" -> Just remainder) = StringErrorParseSuccess (remainder, jsonFalse)
expectValue (T.stripPrefix "null" -> Just remainder) = StringErrorParseSuccess (remainder, jsonNull)
expectValue (T.stripPrefix " " -> Just remainder) = expectValue remainder
expectValue (T.stripPrefix "\r" -> Just remainder) = expectValue remainder
expectValue (T.stripPrefix "\n" -> Just remainder) = expectValue remainder
expectValue (T.stripPrefix "\t" -> Just remainder) = expectValue remainder
expectValue text = expectNumber text


expectObject :: Bool -> M.HashMap JString Json -> T.Text -> StringErrorParseResult (T.Text, Json)
expectObject _ _ "" = StringErrorParseFailure UnexpectedTermination
expectObject _ fields (T.stripPrefix "}" -> Just remainder) = StringErrorParseSuccess (remainder, fromObject $ JObject fields)
expectObject first fields (T.stripPrefix " " -> Just remainder) = expectObject first fields remainder
expectObject first fields (T.stripPrefix "\r" -> Just remainder) = expectObject first fields remainder
expectObject first fields (T.stripPrefix "\t" -> Just remainder) = expectObject first fields remainder
expectObject first fields (T.stripPrefix "\n" -> Just remainder) = expectObject first fields remainder
expectObject first fields text =
                               let result = do afterEntrySeparator <- if first then StringErrorParseSuccess text else expectEntrySeparator text
                                               (afterKey, key) <- expectString afterEntrySeparator
                                               afterFieldSeparator <- expectFieldSeparator afterKey
                                               (afterValue, value) <- expectValue afterFieldSeparator
                                               expectObject False (M.insert (JString key) value fields) afterValue
                               in result


expectArray :: Bool -> V.Vector Json -> T.Text -> StringErrorParseResult (T.Text, Json)
expectArray _ _ (T.null -> True) = StringErrorParseFailure UnexpectedTermination
expectArray _ entries (T.stripPrefix "]" -> Just remainder) = StringErrorParseSuccess (remainder, fromArray $ JArray entries)
expectArray first entries (T.stripPrefix " " -> Just remainder) = expectArray first entries remainder
expectArray first entries (T.stripPrefix "\r" -> Just remainder) = expectArray first entries remainder
expectArray first entries (T.stripPrefix "\t" -> Just remainder) = expectArray first entries remainder
expectArray first entries (T.stripPrefix "\n" -> Just remainder) = expectArray first entries remainder
expectArray first entries text =
                               let result = do afterEntrySeparator <- if first then StringErrorParseSuccess text else expectEntrySeparator text
                                               (afterValue, value) <- expectValue afterEntrySeparator
                                               expectArray False (V.snoc entries value) afterValue
                               in result

expectString :: T.Text -> StringErrorParseResult (T.Text, T.Text)
expectString text = do afterOpen <- expectStringBounds text
                       expectStringNoStartBounds afterOpen

expectStringNoStartBounds :: T.Text -> StringErrorParseResult (T.Text, T.Text)
expectStringNoStartBounds text = do (remainder, textOfString) <- collectStringParts text T.empty
                                    return (remainder, T.reverse textOfString)

expectSpacerToken :: T.Text -> Char -> T.Text -> StringErrorParseResult T.Text
expectSpacerToken (T.null -> True) _ _ = StringErrorParseFailure UnexpectedTermination
expectSpacerToken text expectedToken failMessage = if firstChar == expectedToken then StringErrorParseSuccess remainder else StringErrorParseFailure (ExpectedToken (T.singleton expectedToken) failMessage)
  where
    firstChar = T.head text
    remainder = T.tail text

expectEntrySeparator :: T.Text -> StringErrorParseResult T.Text
expectEntrySeparator text = expectSpacerToken text ',' "Expected entry separator."

expectStringBounds :: T.Text -> StringErrorParseResult T.Text
expectStringBounds text = expectSpacerToken text '"' "Expected string bounds."

expectFieldSeparator :: T.Text -> StringErrorParseResult T.Text
expectFieldSeparator text = expectSpacerToken text ':' "Expected field separator."

collectStringParts :: T.Text -> T.Text -> StringErrorParseResult (T.Text, T.Text)
collectStringParts (T.null -> True) _ = StringErrorParseFailure UnexpectedTermination
collectStringParts (T.stripPrefix "\"" -> Just remainder) workingString = StringErrorParseSuccess (remainder, workingString)
collectStringParts (utfParts -> Just (first, second, third, fourth, remainder)) workingString =
                     let validHex = validUnicodeHex first second third fourth
                         invalidEscapeSequence = StringErrorParseFailure (InvalidEscapeSequence ('\\' `T.cons` 'u' `T.cons` first `T.cons` second `T.cons` third `T.cons` fourth `T.cons` remainder))
                         escapeSequenceValue = unicodeEscapeSequenceValue first second third fourth
                         isSurrogateLead = escapeSequenceValue >= 0xD800 && escapeSequenceValue <= 0xDBFF
                         escapedChar = toEnum escapeSequenceValue
                         surrogateResult = case remainder of
                            (utfParts -> Just (trailFirst, trailSecond, trailThird, trailFourth, trailRemainder)) ->
                              let validTrailHex = validUnicodeHex trailFirst trailSecond trailThird trailFourth
                                  trailEscapeSequenceValue = unicodeEscapeSequenceValue trailFirst trailSecond trailThird trailFourth
                                  isSurrogateTrail = trailEscapeSequenceValue >= 0xDC00 && trailEscapeSequenceValue <= 0xDFFF
                                  surrogatePairChar = toEnum ((shiftL 10 $ escapeSequenceValue - 0xD800) + (trailEscapeSequenceValue - 0xDC00))
                              in if validTrailHex && isSurrogateTrail then (collectStringParts trailRemainder (surrogatePairChar `T.cons` workingString)) else invalidEscapeSequence
                            _ -> invalidEscapeSequence
                         validResult = if isSurrogateLead then surrogateResult else collectStringParts remainder (escapedChar `T.cons` workingString)
                     in if validHex then validResult else invalidEscapeSequence
collectStringParts (T.stripPrefix "\\r" -> Just remainder) workingString = collectStringParts remainder ('\r' `T.cons` workingString)
collectStringParts (T.stripPrefix "\\n" -> Just remainder) workingString = collectStringParts remainder ('\n' `T.cons` workingString)
collectStringParts (T.stripPrefix "\\t" -> Just remainder) workingString = collectStringParts remainder ('\t' `T.cons` workingString)
collectStringParts (T.stripPrefix "\\b" -> Just remainder) workingString = collectStringParts remainder ('\b' `T.cons` workingString)
collectStringParts (T.stripPrefix "\\f" -> Just remainder) workingString = collectStringParts remainder ('\f' `T.cons` workingString)
collectStringParts (T.stripPrefix "\\\\" -> Just remainder) workingString = collectStringParts remainder ('\\' `T.cons` workingString)
collectStringParts (T.stripPrefix "\\/" -> Just remainder) workingString = collectStringParts remainder ('/' `T.cons` workingString)
collectStringParts (T.stripPrefix "\\\"" -> Just remainder) workingString = collectStringParts remainder ('\"' `T.cons` workingString)
collectStringParts (T.stripPrefix "\\" -> Just remainder) _ = StringErrorParseFailure (InvalidEscapeSequence ('\\' `T.cons` remainder))
collectStringParts text workingString = collectStringParts (T.tail text) (T.head text `T.cons` workingString)


utfParts :: T.Text -> Maybe (Char, Char, Char, Char, T.Text)
utfParts (T.stripPrefix "\\u" -> Just rest) = Just (first, second, third, fourth, remainder)
  where
    (first, rest1) = slice rest
    (second,rest2) = slice rest1
    (third,rest3) = slice rest2
    (fourth,remainder) = slice rest3
    slice t = (T.head t, T.tail t)
utfParts _ = Nothing

validUnicodeHex :: Char -> Char -> Char -> Char -> Bool
validUnicodeHex first second third fourth = isHexDigit first && isHexDigit second && isHexDigit third && isHexDigit fourth

unicodeEscapeSequenceValue :: Char -> Char -> Char -> Char -> Int
unicodeEscapeSequenceValue first second third fourth =
    let firstValue = (digitToInt first) `shiftL` 12
        secondValue = (digitToInt second) `shiftL` 8
        thirdValue = (digitToInt third) `shiftL` 4
        fourthValue = digitToInt fourth
    in firstValue .|. secondValue .|. thirdValue .|. fourthValue

isNumberChar :: Char -> Bool
isNumberChar char = isDigit char || char == '+' || char == '-' || char == 'e' || char == 'E' || char == '.'

expectNumber :: T.Text -> StringErrorParseResult (T.Text, Json)
expectNumber text =
                  let (numberPrefix, remainder) = T.span isNumberChar text
                      parsedNumber = readMaybe . T.unpack $ numberPrefix
                      number = parsedNumber >>= fromDouble
                  in maybe (StringErrorParseFailure (InvalidNumberText numberPrefix)) (\n -> StringErrorParseSuccess (remainder, n)) number

