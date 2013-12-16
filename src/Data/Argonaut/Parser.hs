{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}

module Data.Argonaut.Parser
  (
      Parser(..)
    , parse
    , parseString
) where

import Data.Bits
import Data.Char
import Data.Either
import Data.Argonaut
import Control.Monad.Identity
import Text.Read
import Data.Typeable(Typeable)
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as M

class Parser m n a where
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

type EitherStringParseResult = Either (ParseError String)

instance Parser Identity EitherStringParseResult String where
  parseJson (Identity json) = parseString json

validSuffixContent :: String -> Bool
validSuffixContent (' ' : remainder) = validSuffixContent remainder
validSuffixContent ('\r' : remainder) = validSuffixContent remainder
validSuffixContent ('\n' : remainder) = validSuffixContent remainder
validSuffixContent ('\t' : remainder) = validSuffixContent remainder
validSuffixContent [] = True
validSuffixContent _ = False

checkSuffix :: (String, Json) -> EitherStringParseResult Json
checkSuffix (suffix, json) = if validSuffixContent suffix then Right json else Left (InvalidSuffixContent suffix)

parseString :: String -> EitherStringParseResult Json
parseString text = (expectValue text) >>= checkSuffix

expectValue :: String -> EitherStringParseResult (String, Json)
expectValue "" = Left UnexpectedTermination
expectValue ('[' : remainder) = expectArray True V.empty remainder
expectValue ('{' : remainder) = expectObject True M.empty remainder
expectValue ('\"' : remainder) = fmap (\(remainder, string) -> (remainder, fromString string)) (expectStringNoStartBounds remainder)
expectValue ('t' : 'r' : 'u' : 'e' : remainder) = Right (remainder, jsonTrue)
expectValue ('f' : 'a' : 'l' : 's' : 'e' : remainder) = Right (remainder, jsonFalse)
expectValue ('n' : 'u' : 'l' : 'l' : remainder) = Right (remainder, jsonNull)
expectValue (' ' : remainder) = expectValue remainder
expectValue ('\r' : remainder) = expectValue remainder
expectValue ('\n' : remainder) = expectValue remainder
expectValue ('\t' : remainder) = expectValue remainder
expectValue text = expectNumber text


expectObject :: Bool -> JObject -> String -> EitherStringParseResult (String, Json)
expectObject _ _ "" = Left $ UnexpectedTermination
expectObject _ fields ('}' : remainder) = Right (remainder, fromObject fields)
expectObject first fields (' ' : remainder) = expectObject first fields remainder
expectObject first fields ('\r' : remainder) = expectObject first fields remainder
expectObject first fields ('\t' : remainder) = expectObject first fields remainder
expectObject first fields ('\n' : remainder) = expectObject first fields remainder
expectObject first fields text =
                               let result = do afterEntrySeparator <- if first then Right text else expectEntrySeparator text
                                               (afterKey, key) <- expectString afterEntrySeparator
                                               afterFieldSeparator <- expectFieldSeparator afterKey
                                               (afterValue, value) <- expectValue afterFieldSeparator
                                               expectObject False (M.insert key value fields) afterValue
                               in result


expectArray :: Bool -> V.Vector Json -> String -> EitherStringParseResult (String, Json)
expectArray _ _ [] = Left $ UnexpectedTermination
expectArray _ entries (']' : remainder) = Right (remainder, fromArray entries)
expectArray first entries (' ' : remainder) = expectArray first entries remainder
expectArray first entries ('\r' : remainder) = expectArray first entries remainder
expectArray first entries ('\t' : remainder) = expectArray first entries remainder
expectArray first entries ('\n' : remainder) = expectArray first entries remainder
expectArray first entries text =
                               let result = do afterEntrySeparator <- if first then Right text else expectEntrySeparator text
                                               (afterValue, value) <- expectValue afterEntrySeparator
                                               expectArray False (V.snoc entries value) afterValue
                               in result

expectString :: String -> EitherStringParseResult (String, String)
expectString text = do afterOpen <- expectStringBounds text
                       afterString <- expectStringNoStartBounds afterOpen
                       return afterString

expectStringNoStartBounds :: String -> EitherStringParseResult (String, String)
expectStringNoStartBounds text = do (remainder, textOfString) <- collectStringParts text []
                                    return (remainder, reverse textOfString)

expectSpacerToken :: String -> Char -> String -> EitherStringParseResult String
expectSpacerToken (firstChar : remainder) expectedToken failMessage = if firstChar == expectedToken then Right remainder else Left (ExpectedToken (show expectedToken) failMessage)
expectSpacerToken [] _ _ = Left UnexpectedTermination

expectEntrySeparator :: String -> EitherStringParseResult String
expectEntrySeparator text = expectSpacerToken text ',' "Expected entry separator."

expectStringBounds :: String -> EitherStringParseResult String
expectStringBounds text = expectSpacerToken text '"' "Expected string bounds."

expectFieldSeparator :: String -> EitherStringParseResult String
expectFieldSeparator text = expectSpacerToken text ':' "Expected field separator."

collectStringParts :: String -> String -> EitherStringParseResult (String, String)
collectStringParts [] _ = Left UnexpectedTermination
collectStringParts ('\"' : remainder) workingString = Right (remainder, workingString)
collectStringParts ('\\' : 'u' : first : second : third : fourth : remainder) workingString =
                     let validHex = isHexDigit first && isHexDigit second && isHexDigit third && isHexDigit fourth
                         firstValue = (digitToInt first) `shiftL` 12
                         secondValue = (digitToInt second) `shiftL` 8
                         thirdValue = (digitToInt third) `shiftL` 4
                         fourthValue = digitToInt fourth
                         escapedChar = toEnum $ (firstValue .|. secondValue .|. thirdValue .|. fourthValue)
                     in if validHex then (collectStringParts remainder (escapedChar : workingString)) else Left (InvalidEscapeSequence ('\\' : 'u' : first : second : third : fourth : remainder))
collectStringParts ('\\' : 'r' : remainder) workingString = collectStringParts remainder ('\r' : workingString)
collectStringParts ('\\' : 'n' : remainder) workingString = collectStringParts remainder ('\n' : workingString)
collectStringParts ('\\' : 't' : remainder) workingString = collectStringParts remainder ('\t' : workingString)
collectStringParts ('\\' : 'b' : remainder) workingString = collectStringParts remainder ('\b' : workingString)
collectStringParts ('\\' : 'f' : remainder) workingString = collectStringParts remainder ('\f' : workingString)
collectStringParts ('\\' : '\\' : remainder) workingString = collectStringParts remainder ('\\' : workingString)
collectStringParts ('\\' : '/' : remainder) workingString = collectStringParts remainder ('/' : workingString)
collectStringParts ('\\' : '"' : remainder) workingString = collectStringParts remainder ('"' : workingString)
collectStringParts ('\\' : remainder) _ = Left (InvalidEscapeSequence ('\\' : remainder))
collectStringParts (char : remainder) workingString = collectStringParts remainder (char : workingString)

isNumberChar :: Char -> Bool
isNumberChar char = (char >= '0' && char <= '9') || char == '+' || char == '-' || char == 'e' || char == 'E' || char == '.'

expectNumber :: String -> EitherStringParseResult (String, Json)
expectNumber text =
                  let (numberPrefix, remainder) = span isNumberChar text
                      parsedNumber = readMaybe numberPrefix
                      number = parsedNumber >>= fromDouble
                  in maybe (Left (InvalidNumberText numberPrefix)) (\n -> Right (remainder, n)) number

