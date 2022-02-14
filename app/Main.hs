module Main where

import Control.Monad.State
import Data.Char
import Lib

charSize :: Int
charSize = 16

seed = 142857

pseudoRandomBits :: BinStream
pseudoRandomBits = evalState pseudoRandomBitsS (toBinary seed)

grp :: Int -> [a] -> [[a]]
grp _ [] = []
grp n list = (take n list):(grp n (drop n list))

binChars :: BinStream -> String
binChars = map (chr . toDecimal) . grp charSize

main :: IO ()
main = putStrLn $ binChars pseudoRandomBits
