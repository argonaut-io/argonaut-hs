module Main (
    main
 ) where

import qualified Data.Attoparsec.ByteString as ATT
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Foldable
import Data.Monoid
import Data.ByteString(ByteString)
import Data.List
import Data.Traversable
import qualified Data.Argonaut.Parser as ARP
import Data.Argonaut.Printer()
import qualified Data.Aeson.Parser as AEP
import Criterion
import Criterion.Config
import Criterion.Main
import System.Directory

jsonDataDir :: String
jsonDataDir = "./benchmark/json-data/"

toByteString :: String -> ByteString
toByteString = TE.encodeUtf8 . T.pack

main :: IO ()
main = do jsonFilenames <- fmap (filter (isSuffixOf ".json")) $ getDirectoryContents jsonDataDir
          jsonChunks <- traverse (\filename -> fmap (\contents -> (filename, toByteString contents)) $ readFile (jsonDataDir ++ filename)) jsonFilenames
          {-
          _ <- traverse (\(name, bytes) -> do putStrLn name
                                              print (foldMap (Sum . length . show . ARP.parseByteString) (fmap (\_ -> bytes) [1..50]))) jsonChunks
          return ()
          -}
          defaultMainWith benchConfig (return ()) $ benchmarks jsonChunks

benchmarks :: [(String, ByteString)] -> [Benchmark]
benchmarks jsonChunks = fmap (\(name, json) -> 
    bcompare [
        (bgroup "Aeson" $ [bench name $ nf (\j -> show $ (ATT.parse AEP.json' j)) json])
      , (bgroup "Argonaut" $ [bench name $ nf (\j -> show $ (ARP.parseByteString j)) json])
    ]
  ) jsonChunks --(filter (\(name, _) -> isInfixOf "numbers" name) jsonChunks)

benchConfig :: Config
benchConfig = defaultConfig {
             cfgPerformGC = ljust True
           }
