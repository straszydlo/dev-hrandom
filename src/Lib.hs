module Lib where

import Control.Monad.State

type BinDigit = Bool
type BinNumber = [BinDigit]
type BinStream = BinNumber

toDecimal :: BinNumber -> Int
toDecimal = foldl (\acc elem -> 2*acc + if elem then 1 else 0) 0

nextBitS :: State BinNumber BinDigit
nextBitS = do
  current <- get
  let (firstHalf, secondHalf) = splitAt 32 current
  let exponent = toDecimal firstHalf
  let next = modularExp exponent
  let newFirstHalf = drop 32 $ toBinary next
  put $ newFirstHalf ++ secondHalf
  let newBit = foldr xor False (zipWith (&&) firstHalf secondHalf)
  return newBit
  where modularExp :: Int -> Int
        modularExp 0 = 1
        modularExp exponent = (base * modularExp (exponent - 1)) `mod` modulus
        modulus = 2137 --completely arbitrary prime
        base = 10 --smallest generator for Z2137 cyclic group
        xor a b = (a || b) && not (a && b)

toBinary :: Int -> BinNumber
toBinary = pad . reverse . reversedDigits
  where reversedDigits 0 = [False]
        reversedDigits 1 = [True]
        reversedDigits n = (n `mod` 2 == 1):(reversedDigits (n `div` 2))
        pad :: [BinDigit] -> [BinDigit]
        pad digits = replicate (64 - length digits) False ++ digits

