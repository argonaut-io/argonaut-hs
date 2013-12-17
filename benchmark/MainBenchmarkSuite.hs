module Main (
    main
 ) where

import qualified Data.Attoparsec.ByteString as ATT
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.ByteString(ByteString)
import Data.List
import Data.Traversable
import qualified Data.Argonaut.Parser as ARP
import qualified Data.Aeson.Parser as AEP
import Criterion.Config
import Criterion.Main
import System.Directory

main :: IO ()
main = do jsonFilenames <- fmap (filter (isSuffixOf ".json")) $ getDirectoryContents jsonDataDir
          jsonChunks <- traverse (\filename -> fmap (\contents -> (filename, contents, toByteString contents)) $ readFile (jsonDataDir ++ filename)) jsonFilenames
          defaultMainWith benchConfig (return ()) (benchmarks jsonChunks)
       where jsonDataDir = "./benchmark/json-data/"
             toByteString = TE.encodeUtf8 . T.pack

benchmarks :: [(String, String, ByteString)] -> [Benchmark]
benchmarks jsonChunks =
    [
        (bgroup "Argonaut" $ fmap (\(name, json, _) -> bench name $ nf (fmap show $ ARP.parseString) json) jsonChunks)
      , (bgroup "Aeson" $ fmap (\(name, _, json) -> bench name $ nf (\j -> show $ (ATT.parse AEP.json j)) json) jsonChunks)
    ]

benchConfig :: Config
benchConfig = defaultConfig {
             cfgPerformGC = ljust True
           }
