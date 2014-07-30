module Data.Argonaut.Parser where

  import Control.Apply ((<*), (*>))
  import Control.Lens (iso, IsoP())
  import Control.Monad.Identity (Identity(..))

  import Data.Argonaut.Core
    ( fromArray
    , fromNumber
    , fromObject
    , fromString
    , jsonEmptyArray
    , jsonEmptyObject
    , jsonFalse
    , jsonNull
    , jsonTrue
    , Json(..)
    , JArray()
    , JAssoc()
    , JField()
    , JObject()
    )
  import Data.Argonaut.Printer
  import Data.Either (Either(..))
  import Data.Foldable (notElem)
  import Data.Maybe (Maybe(..))
  import Data.String (charCodeAt, joinWith)
  import Data.Tuple (Tuple(..))

  import Global (readFloat)

  import Text.Parsing.Parser (fail, runParser, unParserT, Parser(), ParseError(..), ParserT(..))
  import Text.Parsing.Parser.Combinators ((<?>), many, sepBy, sepBy1, try)
  import Text.Parsing.Parser.String (char, satisfy, string, whiteSpace)

  import qualified Data.Map as M

  foreign import undefined :: forall a. a

  class Parser m n a where
    parseJson :: m a -> n Json

  parseFrom :: forall m a n. (Parser m n a) => m a -> n Json
  parseFrom = parseJson

  data ParseResult a = ParseFailure String
                     | ParseSuccess a

  instance eqParseResult :: (Eq a) => Eq (ParseResult a) where
    (==) (ParseFailure str) (ParseFailure str') = str == str'
    (==) (ParseSuccess a)   (ParseSuccess a')   = a   == a'

    (/=) pr                 pr'                 = not (pr == pr')

  instance showParseResult :: (Show a) => Show (ParseResult a) where
    show (ParseFailure str) = str
    show (ParseSuccess a)   = show a

  instance functorParseResult :: Functor ParseResult where
    (<$>) f x = fromEither (f <$> toEither x)

  instance applyParseResult :: Apply ParseResult where
    (<*>) f x = fromEither (toEither f <*> toEither x)

  instance bindParseResult :: Bind ParseResult where
    (>>=) m f = fromEither (toEither m >>= (f >>> toEither))

  instance applicativeParseResult :: Applicative ParseResult where
    pure = ParseSuccess

  instance monadParseResult :: Monad ParseResult

  instance parserIdParseResultString :: Parser Identity ParseResult String where
    parseJson (Identity str) = parseString str

  -- Constants
  closeBrace = "}"
  closeBracket = "]"
  comma = ","
  doubleQuote = "\""
  openBrace = "{"
  openBracket = "["

  parseMaybe :: forall a. (Parser Identity ParseResult a) => a -> Maybe Json
  parseMaybe x = case parseJson (Identity x) of
    ParseFailure _ -> Nothing
    ParseSuccess x -> Just x

  parseString :: String -> ParseResult Json
  parseString str = case runParser str jsonParser of
    Left  (ParseError {message = err})  -> ParseFailure err
    Right json -> ParseSuccess json

  jsonParser :: Parser String Json
  jsonParser = do
    skipSpaces
    c <- lookAhead char
    case c of
      "{" -> objectParser unit
      "[" -> arrayParser unit
      _   -> invalidJson "object or array"

  objectParser :: Unit -> Parser String Json
  objectParser _ = try emptyObjectParser
               <|> nonEmptyObjectParser unit

  arrayParser :: Unit -> Parser String Json
  arrayParser _ = try emptyArrayParser
              <|> nonEmptyArrayParser

  emptyObjectParser :: Parser String Json
  emptyObjectParser = skipSpaces *> braces (skipSpaces *> pure jsonEmptyObject)

  nonEmptyObjectParser :: Unit -> Parser String Json
  nonEmptyObjectParser _ =
    skipSpaces *> braces (skipSpaces *> membersParser unit <* skipSpaces)

  membersParser :: Unit -> Parser String Json
  membersParser _ =
    (M.fromList >>> fromObject) <$> sepBy1 (memberParser unit) (string comma)

  memberParser :: Unit -> Parser String JAssoc
  memberParser _ = do
    skipSpaces
    key <- rawStringParser
    skipSpaces
    string ":"
    skipSpaces
    val <- valueParser unit
    pure $ Tuple key val

  emptyArrayParser :: Parser String Json
  emptyArrayParser = skipSpaces *> brackets (skipSpaces *> pure jsonEmptyArray)

  nonEmptyArrayParser :: Parser String Json
  nonEmptyArrayParser = do
    skipSpaces
    fromArray <$> brackets (skipSpaces *> sepBy (valueParser unit) (string comma) <* skipSpaces)

  nullParser :: Parser String Json
  nullParser = skipSpaces *> string "null" *> pure jsonNull

  booleanParser :: Parser String Json
  booleanParser = do
    skipSpaces
    b <- lookAhead char
    case b of
      "t" -> string "true"  *> pure jsonTrue
      "f" -> string "false" *> pure jsonFalse
      _   -> invalidJson "one of 'true' or 'false'"

  numberParser :: Parser String Json
  numberParser = do
    skipSpaces
    neg <- option "" $ string "-"
    d <- lookAhead char
    d' <- case d of
      "0"             -> char
      _ | oneToNine d -> digits
      _               -> invalidJson "digit"
    frac <- option "" $ fracParser
    exp <- option "" $ expParser
    pure $ fromNumber $ readFloat $ neg ++ d' ++ frac ++ exp

  digits :: Parser String String
  digits =
    joinWith "" <$> manyTill digit (lookAhead $ satisfy $ not <<< isDigit)

  digit :: Parser String String
  digit = satisfy isDigit

  fracParser :: Parser String String
  fracParser = do
    string "."
    digits' <- digits
    pure ("." ++ digits')

  expParser :: Parser String String
  expParser = do
    e <- try (string "e") <|> string "E"
    sign <- option "" (try (string "+") <|> try (string "-"))
    digits' <- digits
    pure (e ++ sign ++ digits')

  stringParser :: Parser String Json
  stringParser = fromString <$> rawStringParser

  rawStringParser :: Parser String String
  rawStringParser = do
    skipSpaces
    string "\""
    key <- joinWith "" <$> manyTill char (lookAhead $ string "\"")
    string "\""
    pure key

  valueParser :: Unit -> Parser String Json
  valueParser _ = try nullParser
              <|> try booleanParser
              <|> try stringParser
              <|> try (objectParser unit)
              <|> try (arrayParser unit)
              <|> try numberParser
              <|> invalidJson "valid JSON"

  invalidJson :: forall a. String -> Parser String a
  invalidJson expected = many char >>= \s -> fail $ "Invalid JSON:\n\t" ++
    "Expected " ++ expected ++ ".\n\t" ++
    "Found: " ++ joinWith "" s

  -- String things.

  ord :: String -> Number
  ord = charCodeAt 0

  isDigit :: String -> Boolean
  isDigit str | 48 <= ord str && ord str <= 57 = true
  isDigit _                                    = false

  oneToNine :: String -> Boolean
  oneToNine str = isDigit str && str /= "0"

  -- Parser things. Should move them to purescript-parsing

  type ParserState s a =
    { input    :: s
    , result   :: Either ParseError a
    , consumed :: Boolean
    }

  skipSpaces :: Parser String {}
  skipSpaces = whiteSpace *> pure {}

  skipMany :: forall s a m. (Monad m) => ParserT s m a -> ParserT s m {}
  skipMany p = skipMany1 p <|> pure {}

  skipMany1 :: forall s a m. (Monad m) => ParserT s m a -> ParserT s m {}
  skipMany1 p = do
    x <- p
    xs <- skipMany p
    pure {}

  lookAhead :: forall s a m. (Monad m) => ParserT s m a -> ParserT s m a
  lookAhead (ParserT p) = ParserT \s -> do
    state <- p s
    pure state{input = s, consumed = false}

  instance showParseError :: Show ParseError where
    show (ParseError msg) = msg.message

  noneOf :: forall s m a. (Monad m) => [String] -> ParserT String m String
  noneOf ss = satisfy (flip notElem ss)

  manyTill :: forall s a m e. (Monad m) => ParserT s m a -> ParserT s m e -> ParserT s m [a]
  manyTill p end = scan
    where
      scan = (do
                end
                pure [])
         <|> (do
                x <- p
                xs <- scan
                pure (x:xs))

  option :: forall s a m. (Monad m) => a -> ParserT s m a -> ParserT s m a
  option def p = try p <|> pure def

  between :: forall s a m open close. (Monad m) => ParserT s m open -> ParserT s m close -> ParserT s m a -> ParserT s m a
  between open close p = open *> p <* close

  braces :: forall s m a. (Monad m) => ParserT String m a -> ParserT String m a
  braces = between (string openBrace) (string closeBrace)

  brackets :: forall m a. (Monad m) => ParserT String m a -> ParserT String m a
  brackets = between (string openBracket) (string closeBracket)

  quoted :: forall m a. (Monad m) => ParserT String m a -> ParserT String m a
  quoted = between (string doubleQuote) (string doubleQuote)

  -- Iso between `ParseResult a` and `Either String a`

  isoParseEither :: forall a. IsoP (ParseResult a) (Either String a)
  isoParseEither = iso toEither fromEither

  toEither :: forall a. ParseResult a -> Either String a
  toEither (ParseFailure str) = Left str
  toEither (ParseSuccess x)   = Right x

  fromEither :: forall a. Either String a -> ParseResult a
  fromEither (Left str) = ParseFailure str
  fromEither (Right x)  = ParseSuccess x
