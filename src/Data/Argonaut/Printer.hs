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

module Data.Argonaut.Printer
  (
      Printer(..)
    , printTo
    , printIdentity
    , printToByteString
    , printToText
    , printToString
) where

import Data.Monoid
import Data.Char
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Builder.Prim as BSBP
import Data.Argonaut.Core
import Control.Monad.Identity
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Scientific (Scientific, coefficient, base10Exponent, formatScientific, FPFormat(Generic))

data PrinterConfig = PrinterConfig {
    lbraceLeft        :: Int -> String
  , lbraceRight       :: Int -> String
  , rbraceLeft        :: Int -> String
  , rbraceRight       :: Int -> String
  , lbracketLeft      :: Int -> String
  , lbracketRight     :: Int -> String
  , rbracketLeft      :: Int -> String
  , rbracketRight     :: Int -> String
  , commaLeft         :: Int -> String
  , commaRight        :: Int -> String
  , colonLeft         :: Int -> String
  , colonRight        :: Int -> String
}

class Printer m n a | m a -> n where
  printJson :: m Json -> n a

printTo :: Printer m n a => m Json -> n a
printTo = printJson

printIdentity :: Printer Identity Identity a => Json -> a
printIdentity = runIdentity . printTo . Identity

openBraceBuilder :: BSB.Builder
openBraceBuilder = BSB.charUtf8 '{'

closeBraceBuilder :: BSB.Builder
closeBraceBuilder = BSB.charUtf8 '}'

openSquareBuilder :: BSB.Builder
openSquareBuilder = BSB.charUtf8 '['

closeSquareBuilder :: BSB.Builder
closeSquareBuilder = BSB.charUtf8 ']'

emptyArrayBuilder :: BSB.Builder
emptyArrayBuilder = openSquareBuilder `mappend` closeSquareBuilder

commaBuilder :: BSB.Builder
commaBuilder = BSB.charUtf8 ','

colonBuilder :: BSB.Builder
colonBuilder = BSB.charUtf8 ':'

trueBuilder :: BSB.Builder
trueBuilder = BSB.stringUtf8 "true"

falseBuilder :: BSB.Builder
falseBuilder = BSB.stringUtf8 "false"

nullBuilder :: BSB.Builder
nullBuilder = BSB.stringUtf8 "null"

speechMarkBuilder :: BSB.Builder
speechMarkBuilder = BSB.charUtf8 '"'

escapedCarriageReturnBuilder :: BSB.Builder
escapedCarriageReturnBuilder = BSB.stringUtf8 "\\r"

escapedLineFeedBuilder :: BSB.Builder
escapedLineFeedBuilder = BSB.stringUtf8 "\\n"

escapedTabBuilder :: BSB.Builder
escapedTabBuilder = BSB.stringUtf8 "\\t"

escapedBackSlashBuilder :: BSB.Builder
escapedBackSlashBuilder = BSB.stringUtf8 "\\\\"

escapedSpeechMarkBuilder :: BSB.Builder
escapedSpeechMarkBuilder = BSB.stringUtf8 "\\\""

slashUBuilder :: BSB.Builder
slashUBuilder = BSB.stringUtf8 "\\u"

arrayToBuilder :: JArray -> BSB.Builder
arrayToBuilder vector
  | V.null vector  = emptyArrayBuilder
  | otherwise = openSquareBuilder `mappend` encodeToByteStringBuilder (V.unsafeHead vector) `mappend` V.foldr' withComma (BSB.char8 ']') (V.unsafeTail vector)
  where
    withComma a z = commaBuilder `mappend` encodeToByteStringBuilder a `mappend` z

objectToBuilder :: JObject -> BSB.Builder
objectToBuilder objectMap = case M.toList objectMap of
    (x:xs) -> openBraceBuilder `mappend` one x `mappend` foldr withComma closeBraceBuilder xs
    _ -> openBraceBuilder `mappend` closeBraceBuilder
  where
    withComma a z = commaBuilder `mappend` one a `mappend` z
    one (k,v) = stringToBuilder k `mappend` colonBuilder `mappend` encodeToByteStringBuilder v

isNormalChar :: Char -> Bool
isNormalChar char = case () of _
                                 | char == '\\'   -> False
                                 | char == '\"'   -> False
                                 | isControl char -> False
                                 | otherwise      -> True

isNotNormalChar :: Char -> Bool
isNotNormalChar = not . isNormalChar

appendToBuilder :: BSB.Builder -> Char -> BSB.Builder
appendToBuilder builder char = 
  let toAppend    = case () of _
                                 | char == '\\'   -> escapedBackSlashBuilder
                                 | char == '\"'   -> escapedSpeechMarkBuilder
                                 | char == '\r'   -> escapedCarriageReturnBuilder
                                 | char == '\n'   -> escapedLineFeedBuilder
                                 | char == '\t'   -> escapedTabBuilder
                                 | isControl char -> hexEscaped
                                 | otherwise      -> BSB.charUtf8 char
      hexEscaped  = slashUBuilder `mappend` BSBP.primFixed BSBP.int16HexFixed (fromIntegral $ ord char)
  in  {-# SCC appendToBuilder #-} builder `mappend` toAppend

stringToBuilder :: JString -> BSB.Builder
stringToBuilder text = {-# SCC stringToBuilder #-} appendNormalChars text speechMarkBuilder `mappend` speechMarkBuilder
  where
    appendNormalChars :: T.Text -> BSB.Builder -> BSB.Builder
    appendNormalChars appendText builder | T.null appendText    = builder
    appendNormalChars appendText builder                        =
      let (prefix, suffix)    = T.span isNormalChar appendText
          builderWithPrefix   = builder `mappend` (BSB.byteString $ TE.encodeUtf8 prefix)
      in  appendNotNormalChars suffix builderWithPrefix
    appendNotNormalChars :: T.Text -> BSB.Builder -> BSB.Builder
    appendNotNormalChars appendText builder | T.null appendText = builder
    appendNotNormalChars appendText builder                     =
      let (prefix, suffix)    = T.span isNotNormalChar appendText
          builderWithPrefix   = T.foldl' appendToBuilder builder prefix
      in  appendNormalChars suffix builderWithPrefix

numberToBuilder :: Scientific -> BSB.Builder
numberToBuilder s
    | e < 0       = BSB.string8 $ formatScientific Generic Nothing s
    | otherwise   = BSB.integerDec (coefficient s * 10 ^ e)
  where
    e = base10Exponent s

encodeToByteStringBuilder :: Json -> BSB.Builder
encodeToByteStringBuilder = {-# SCC encodeToByteStringBuilder #-} foldJson nullBuilder (\bool -> if bool then trueBuilder else falseBuilder) numberToBuilder stringToBuilder arrayToBuilder objectToBuilder

instance Printer Identity Identity BSB.Builder where 
  printJson = Identity . encodeToByteStringBuilder . runIdentity

instance Printer Identity Identity B.ByteString where 
  printJson = fmap (LB.toStrict . BSB.toLazyByteString) . printTo

instance Printer Identity Identity T.Text where 
  printJson = fmap TE.decodeUtf8 . printTo

instance Printer Identity Identity String where 
  printJson = fmap T.unpack . printTo

printToByteString :: Json -> B.ByteString
printToByteString = printIdentity

printToText :: Json -> T.Text
printToText = printIdentity

printToString :: Json -> String
printToString = T.unpack . printIdentity

instance Show Json where
  show = printToString