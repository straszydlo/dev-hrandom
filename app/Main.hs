module Main where

import Control.Monad.State
import Data.Char
import Data.Time.Clock.System
import Lib

charSize :: Int
charSize = 16

timeBasedSeed :: IO Int
timeBasedSeed = fromIntegral . systemSeconds <$> getSystemTime

pseudoRandomBits :: Int -> BinStream
pseudoRandomBits seed = evalState pseudoRandomBitsS (toBinary seed)

grp :: Int -> [a] -> [[a]]
grp _ [] = []
grp n list = (take n list):(grp n (drop n list))

binChars :: BinStream -> String
binChars = map (chr . toDecimal) . grp charSize

main :: IO ()
main = do
  seed <- timeBasedSeed
  putStrLn $ binChars $ pseudoRandomBits seed

