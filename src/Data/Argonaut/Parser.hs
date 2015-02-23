{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Argonaut.Parser
  (
      Parser(..)
    , parseMaybe
    , parseFrom
    , parseString
    , parseText
    , parseByteString
    , ParseResult(..)
    , jsonValidSuffixParser
    , jsonEOFParser
    , parseL
) where

import Data.Bits
import Data.Word
import Data.Monoid
import Data.Char (chr)
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as BU
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Builder as BSB
import Data.Argonaut.Core
import Data.Argonaut.Printer
import Control.Lens
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Argonaut.Templates()
import qualified Data.Attoparsec.ByteString as A
import qualified Data.Attoparsec.ByteString.Lazy as L
import qualified Data.Attoparsec.Zepto as Z
import Control.Applicative ((*>), (<$>), (<*), liftA2, pure)
import qualified Data.Attoparsec.ByteString.Char8 as AC

#define BACKSLASH 92
#define CLOSE_CURLY 125
#define CLOSE_SQUARE 93
#define COMMA 44
#define DOUBLE_QUOTE 34
#define OPEN_CURLY 123
#define OPEN_SQUARE 91
#define C_0 48
#define C_9 57
#define C_A 65
#define C_F 70
#define C_a 97
#define C_f 102
#define C_n 110
#define C_t 116

class Parser m n a | m a -> n where
  parseJson :: m a -> n Json

parseFrom :: Parser m n a => m a -> n Json
parseFrom = parseJson

data ParseResult a = ParseFailure !String | ParseSuccess !a deriving (Eq, Show)

instance Functor ParseResult where
  fmap _ (ParseFailure x)  = ParseFailure x
  fmap f (ParseSuccess y)  = ParseSuccess (f y)

instance Monad ParseResult where
  return = ParseSuccess
  ParseFailure l >>= _ = ParseFailure l
  ParseSuccess r >>= k = k r

instance Parser Identity ParseResult String where
  parseJson (Identity json) = parseString json

instance Parser Identity ParseResult B.ByteString where
  parseJson (Identity json) = parseByteString json

instance Parser Identity ParseResult T.Text where
  parseJson (Identity json) = parseText json

parseMaybe :: Parser Identity ParseResult a => a -> Maybe Json
parseMaybe value = case parseJson (Identity value) of
  ParseFailure _      -> Nothing
  ParseSuccess result -> Just result

parseString :: String -> ParseResult Json
parseString = parseText . T.pack

parseText :: T.Text -> ParseResult Json
parseText = parseByteString . TE.encodeUtf8

parseByteString :: B.ByteString -> ParseResult Json
parseByteString bytestring = case A.parseOnly jsonValidSuffixParser bytestring of
  Left failMessage      -> ParseFailure failMessage
  Right result          -> ParseSuccess result

objectParser :: A.Parser Json
objectParser = do
  !vals <- objectValuesParser quotedStringParser valueParser
  return (fromObject vals)

objectValuesParser :: A.Parser T.Text -> A.Parser Json -> A.Parser (M.HashMap JString Json)
objectValuesParser str val = do
  AC.skipSpace
  let pair = liftA2 (,) (str <* AC.skipSpace) (AC.char ':' *> AC.skipSpace *> val)
  M.fromList <$> commaSeparated pair CLOSE_CURLY
{-# INLINE objectValuesParser #-}

arrayParser :: A.Parser Json
arrayParser = do
  !vals <- arrayValuesParser valueParser
  return (fromArray vals)

commaSeparated :: A.Parser a -> Word8 -> A.Parser [a]
commaSeparated item endByte = do
  w <- L.peekWord8'
  if w == endByte
    then A.anyWord8 >> return []
    else loop
  where
    loop = do
      v <- item <* AC.skipSpace
      ch <- A.satisfy $ \w -> w == COMMA || w == endByte
      if ch == COMMA
        then AC.skipSpace >> (v:) <$> loop
        else return [v]
{-# INLINE commaSeparated #-}

arrayValuesParser :: A.Parser Json -> A.Parser (V.Vector Json)
arrayValuesParser val = do
  AC.skipSpace
  V.fromList <$> commaSeparated val CLOSE_SQUARE
{-# INLINE arrayValuesParser #-}

valueParser :: A.Parser Json
valueParser = do
  w <- L.peekWord8'
  case w of
    DOUBLE_QUOTE -> do
                     !s <- A.anyWord8 *> stringWithoutLeadingQuoteParser
                     return (fromText s)
    OPEN_CURLY -> A.anyWord8 *> objectParser
    OPEN_SQUARE -> A.anyWord8 *> arrayParser
    C_f -> AC.string "false" *> pure jsonFalse
    C_t -> AC.string "true" *> pure jsonTrue
    C_n -> AC.string "null" *> pure jsonNull
    _ | w >= 48 && w <= 57 || w == 45
                  -> do
                     !n <- AC.rational
                     return (fromScientific n)
      | otherwise -> fail "Not a valid JSON value."

-- | Parse a quoted JSON string.
quotedStringParser :: A.Parser T.Text
quotedStringParser = A.word8 DOUBLE_QUOTE *> stringWithoutLeadingQuoteParser

-- | Parse a string without a leading quote.
stringWithoutLeadingQuoteParser :: A.Parser T.Text
stringWithoutLeadingQuoteParser = do
  s <- A.scan False $ \s c -> if s then Just False
                                   else if c == DOUBLE_QUOTE
                                        then Nothing
                                        else Just (c == BACKSLASH)
  _ <- A.word8 DOUBLE_QUOTE
  s1 <- if BACKSLASH `B.elem` s
        then case Z.parse unescape s of
            Right r -> return r
            Left err -> fail err
         else return s

  case TE.decodeUtf8' s1 of
      Right r -> return r
      Left err -> fail $ show err

unescape :: Z.Parser B.ByteString
unescape = toByteString <$> go mempty where
  go acc = do
    h <- Z.takeWhile (/=BACKSLASH)
    let rest = do
          start <- Z.take 2
          let !slash = BU.unsafeHead start
              !t = BU.unsafeIndex start 1
              escape = case B.findIndex (==t) "\"\\/ntbrfu" of
                         Just i -> i
                         _ -> 255
          if slash /= BACKSLASH || escape == 255
            then fail "Invalid JSON escape sequence."
            else do
            let cont m = go (acc `mappend` BSB.byteString h `mappend` m)
                {-# INLINE cont #-}
            if t /= 117 -- 'u'
              then cont (BSB.word8 (BU.unsafeIndex mappingEscapeChars escape))
              else do
                   a <- hexQuad
                   if a < 0xd800 || a > 0xdfff
                     then cont (BSB.charUtf8 (chr a))
                     else do
                       b <- Z.string "\\u" *> hexQuad
                       if a <= 0xdbff && b >= 0xdc00 && b <= 0xdfff
                         then let !c = ((a - 0xd800) `shiftL` 10) +
                                       (b - 0xdc00) + 0x10000
                              in cont (BSB.charUtf8 (chr c))
                         else fail "Invalid UTF-16 surrogates."
    done <- Z.atEnd
    if done
      then return (acc `mappend` BSB.byteString h)
      else rest
  mappingEscapeChars = "\"\\/\n\t\b\r\f"

hexQuad :: Z.Parser Int
hexQuad = do
  s <- Z.take 4
  let hex n | w >= C_0 && w <= C_9 = w - C_0
            | w >= C_a && w <= C_f = w - 87
            | w >= C_A && w <= C_F = w - 55
            | otherwise = 255
        where w = fromIntegral $ BU.unsafeIndex s n
      a = hex 0; b = hex 1; c = hex 2; d = hex 3
  if (a .|. b .|. c .|. d) /= 255
    then return $! d .|. (c `shiftL` 4) .|. (b `shiftL` 8) .|. (a `shiftL` 12)
    else fail "Invalid hex escape."

jsonValidSuffixParser :: A.Parser Json
jsonValidSuffixParser = valueParser <* AC.skipSpace

jsonEOFParser :: A.Parser Json
jsonEOFParser = jsonValidSuffixParser <* AC.endOfInput

toByteString :: BSB.Builder -> B.ByteString
toByteString = LB.toStrict . BSB.toLazyByteString
{-# INLINE toByteString #-}

parseL :: (Parser Identity ParseResult a, Printer Identity Identity a) => Prism' a Json
parseL = prism' printIdentity parseMaybe
