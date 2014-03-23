{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Data.Argonaut.Templates where

import qualified Control.Monad as M
import qualified Data.Traversable as T
import Language.Haskell.TH

buildWord8Dec :: String -> Integer -> [Q Dec]
buildWord8Dec name word = 
  let numberLit   = integerL word
      funName     = mkName (name ++ "Char")
      definition  = valD (varP funName) (normalB $ litE $ numberLit) []
      signature   = sigD funName (conT $ mkName "Word8")
  in  [definition, signature] 

buildWord8s :: [(String, Integer)] -> Q [Dec]
buildWord8s values = T.sequence $ M.join $ fmap (\(name, word) -> buildWord8Dec name word) values
