module Data.Argonaut.Parser (jsonParser) where
  import Data.Argonaut.Core

  import Data.Function
  import Data.Either

  foreign import _jsonParser 
    "function _jsonParser(fail, succ, s) {\
    \   try { return succ(JSON.parse(s)); } catch (e) { return fail(e.message); }\
    \}" :: forall a. Fn3 (String -> a) (Json -> a) String a

  jsonParser :: String -> Either String Json
  jsonParser j = runFn3 _jsonParser Left Right j