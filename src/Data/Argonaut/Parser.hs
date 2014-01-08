{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Data.Argonaut.Parser
  (
      Parser(..)
    , parse
    , parseString
    , ParserInputString
    , StringErrorParseResult(..)
) where

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

newtype ParserInputString = ParserInputString String deriving (Eq, Ord, Show)

runInputString :: ParserInputString -> String
runInputString (ParserInputString value) = value

data StringErrorParseResult a = StringErrorParseFailure !(ParseError String) | StringErrorParseSuccess !a deriving (Eq, Show)

instance Functor StringErrorParseResult where
  fmap _ (StringErrorParseFailure x) = StringErrorParseFailure x
  fmap f (StringErrorParseSuccess y) = StringErrorParseSuccess (f y)

instance Monad StringErrorParseResult where
  return = StringErrorParseSuccess
  StringErrorParseFailure l >>= _ = StringErrorParseFailure l
  StringErrorParseSuccess r >>= k = k r

instance Parser Identity StringErrorParseResult ParserInputString where
  parseJson (Identity json) = parseString $ runInputString json

validSuffixContent :: String -> Bool
validSuffixContent (' ' : remainder) = validSuffixContent remainder
validSuffixContent ('\r' : remainder) = validSuffixContent remainder
validSuffixContent ('\n' : remainder) = validSuffixContent remainder
validSuffixContent ('\t' : remainder) = validSuffixContent remainder
validSuffixContent [] = True
validSuffixContent _ = False

checkSuffix :: (String, Json) -> StringErrorParseResult Json
checkSuffix (suffix, json) = if validSuffixContent suffix then StringErrorParseSuccess json else StringErrorParseFailure (InvalidSuffixContent suffix)

parseString :: String -> StringErrorParseResult Json
parseString text = (expectValue text) >>= checkSuffix

expectValue :: String -> StringErrorParseResult (String, Json)
expectValue "" = StringErrorParseFailure UnexpectedTermination
expectValue ('[' : remainder) = expectArray True V.empty remainder
expectValue ('{' : remainder) = expectObject True M.empty remainder
expectValue ('\"' : remainder) = fmap (\(r, string) -> (r, fromString string)) (expectStringNoStartBounds remainder)
expectValue ('t' : 'r' : 'u' : 'e' : remainder) = StringErrorParseSuccess (remainder, jsonTrue)
expectValue ('f' : 'a' : 'l' : 's' : 'e' : remainder) = StringErrorParseSuccess (remainder, jsonFalse)
expectValue ('n' : 'u' : 'l' : 'l' : remainder) = StringErrorParseSuccess (remainder, jsonNull)
expectValue (' ' : remainder) = expectValue remainder
expectValue ('\r' : remainder) = expectValue remainder
expectValue ('\n' : remainder) = expectValue remainder
expectValue ('\t' : remainder) = expectValue remainder
expectValue text = expectNumber text


expectObject :: Bool -> M.HashMap JString Json -> String -> StringErrorParseResult (String, Json)
expectObject _ _ "" = StringErrorParseFailure $ UnexpectedTermination
expectObject _ fields ('}' : remainder) = StringErrorParseSuccess (remainder, fromObject $ JObject fields)
expectObject first fields (' ' : remainder) = expectObject first fields remainder
expectObject first fields ('\r' : remainder) = expectObject first fields remainder
expectObject first fields ('\t' : remainder) = expectObject first fields remainder
expectObject first fields ('\n' : remainder) = expectObject first fields remainder
expectObject first fields text =
                               let result = do afterEntrySeparator <- if first then StringErrorParseSuccess text else expectEntrySeparator text
                                               (afterKey, key) <- expectString afterEntrySeparator
                                               afterFieldSeparator <- expectFieldSeparator afterKey
                                               (afterValue, value) <- expectValue afterFieldSeparator
                                               expectObject False (M.insert (JString key) value fields) afterValue
                               in result


expectArray :: Bool -> V.Vector Json -> String -> StringErrorParseResult (String, Json)
expectArray _ _ [] = StringErrorParseFailure $ UnexpectedTermination
expectArray _ entries (']' : remainder) = StringErrorParseSuccess (remainder, fromArray $ JArray entries)
expectArray first entries (' ' : remainder) = expectArray first entries remainder
expectArray first entries ('\r' : remainder) = expectArray first entries remainder
expectArray first entries ('\t' : remainder) = expectArray first entries remainder
expectArray first entries ('\n' : remainder) = expectArray first entries remainder
expectArray first entries text =
                               let result = do afterEntrySeparator <- if first then StringErrorParseSuccess text else expectEntrySeparator text
                                               (afterValue, value) <- expectValue afterEntrySeparator
                                               expectArray False (V.snoc entries value) afterValue
                               in result

expectString :: String -> StringErrorParseResult (String, String)
expectString text = do afterOpen <- expectStringBounds text
                       afterString <- expectStringNoStartBounds afterOpen
                       return afterString

expectStringNoStartBounds :: String -> StringErrorParseResult (String, String)
expectStringNoStartBounds text = do (remainder, textOfString) <- collectStringParts text []
                                    return (remainder, reverse textOfString)

expectSpacerToken :: String -> Char -> String -> StringErrorParseResult String
expectSpacerToken (firstChar : remainder) expectedToken failMessage = if firstChar == expectedToken then StringErrorParseSuccess remainder else StringErrorParseFailure (ExpectedToken (show expectedToken) failMessage)
expectSpacerToken [] _ _ = StringErrorParseFailure UnexpectedTermination

expectEntrySeparator :: String -> StringErrorParseResult String
expectEntrySeparator text = expectSpacerToken text ',' "Expected entry separator."

expectStringBounds :: String -> StringErrorParseResult String
expectStringBounds text = expectSpacerToken text '"' "Expected string bounds."

expectFieldSeparator :: String -> StringErrorParseResult String
expectFieldSeparator text = expectSpacerToken text ':' "Expected field separator."

collectStringParts :: String -> String -> StringErrorParseResult (String, String)
collectStringParts [] _ = StringErrorParseFailure UnexpectedTermination
collectStringParts ('\"' : remainder) workingString = StringErrorParseSuccess (remainder, workingString)
collectStringParts ('\\' : 'u' : first : second : third : fourth : remainder) workingString =
                     let validHex = validUnicodeHex first second third fourth
                         invalidEscapeSequence = StringErrorParseFailure (InvalidEscapeSequence ('\\' : 'u' : first : second : third : fourth : remainder))
                         escapeSequenceValue = unicodeEscapeSequenceValue first second third fourth
                         isSurrogateLead = escapeSequenceValue >= 0xD800 && escapeSequenceValue <= 0xDBFF
                         escapedChar = toEnum $ escapeSequenceValue
                         surrogateResult = case remainder of
                            '\\' : 'u' : trailFirst : trailSecond : trailThird : trailFourth : trailRemainder ->
                              let validTrailHex = validUnicodeHex trailFirst trailSecond trailThird trailFourth
                                  trailEscapeSequenceValue = unicodeEscapeSequenceValue trailFirst trailSecond trailThird trailFourth
                                  isSurrogateTrail = trailEscapeSequenceValue >= 0xDC00 && trailEscapeSequenceValue <= 0xDFFF
                                  surrogatePairChar = toEnum ((shiftL 10 $ escapeSequenceValue - 0xD800) + (trailEscapeSequenceValue - 0xDC00))
                              in if validTrailHex && isSurrogateTrail then (collectStringParts trailRemainder (surrogatePairChar : workingString)) else invalidEscapeSequence
                            _ -> invalidEscapeSequence
                         validResult = if isSurrogateLead then surrogateResult else collectStringParts remainder (escapedChar : workingString)
                     in if validHex then validResult else invalidEscapeSequence
collectStringParts ('\\' : 'r' : remainder) workingString = collectStringParts remainder ('\r' : workingString)
collectStringParts ('\\' : 'n' : remainder) workingString = collectStringParts remainder ('\n' : workingString)
collectStringParts ('\\' : 't' : remainder) workingString = collectStringParts remainder ('\t' : workingString)
collectStringParts ('\\' : 'b' : remainder) workingString = collectStringParts remainder ('\b' : workingString)
collectStringParts ('\\' : 'f' : remainder) workingString = collectStringParts remainder ('\f' : workingString)
collectStringParts ('\\' : '\\' : remainder) workingString = collectStringParts remainder ('\\' : workingString)
collectStringParts ('\\' : '/' : remainder) workingString = collectStringParts remainder ('/' : workingString)
collectStringParts ('\\' : '"' : remainder) workingString = collectStringParts remainder ('"' : workingString)
collectStringParts ('\\' : remainder) _ = StringErrorParseFailure (InvalidEscapeSequence ('\\' : remainder))
collectStringParts (char : remainder) workingString = collectStringParts remainder (char : workingString)

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
isNumberChar char = (char >= '0' && char <= '9') || char == '+' || char == '-' || char == 'e' || char == 'E' || char == '.'

expectNumber :: String -> StringErrorParseResult (String, Json)
expectNumber text =
                  let (numberPrefix, remainder) = span isNumberChar text
                      parsedNumber = readMaybe numberPrefix
                      number = parsedNumber >>= fromDouble
                  in maybe (StringErrorParseFailure (InvalidNumberText numberPrefix)) (\n -> StringErrorParseSuccess (remainder, n)) number

