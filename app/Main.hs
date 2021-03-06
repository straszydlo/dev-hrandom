module Main where

import Control.Monad.State
import Data.Char
import Data.List.Split
import Data.Text.Lazy hiding (chunksOf, drop, map, take)
import qualified Data.Text.Lazy.IO as Text(putStr)
import Data.Time.Clock.System
import Lib
import Main.Utf8

charSize :: Int
charSize = 16

timeBasedSeed :: IO Int
timeBasedSeed = fromIntegral . systemSeconds <$> getSystemTime

pseudoRandomBits :: Int -> BinStream
pseudoRandomBits seed = evalState pseudoRandomBitsS (toBinary seed)

binChars :: BinStream -> String
binChars = map (chr . toDecimal) . chunksOf charSize

main :: IO ()
main = withUtf8 $ do
  seed <- timeBasedSeed
  Text.putStr $ pack $ binChars $ pseudoRandomBits seed

