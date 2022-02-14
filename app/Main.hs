module Main where

import Control.Monad.State
import Data.Char
import Lib

charSize :: Int
charSize = 16

bits :: BinStream
bits = cycle [False, False, False, False, False, False, False, False, False, True, False, False, False, False, False, True] --infinite stream of 65s encoded in a pseudo-binary fashion, to be replaced with real implementation

grp :: Int -> [a] -> [[a]]
grp _ [] = []
grp n list = (take n list):(grp n (drop n list))

binChars :: BinStream -> String
binChars = map (chr . toDecimal) . grp charSize

main :: IO ()
main = putStrLn $ binChars bits
