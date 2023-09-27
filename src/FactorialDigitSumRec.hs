module FactorialDigitSumRec(doMain) where

import Data.Char

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

digitSum :: Integer -> Int
digitSum n = iterateThrough (show n)
    where
        iterateThrough :: String -> Int
        iterateThrough [] = 0
        iterateThrough (s:str) = ord s - 48 + iterateThrough str

factorialDigitSum :: Integer -> Int
factorialDigitSum n = digitSum (factorial n)

doMain :: IO ()
doMain = print $ factorialDigitSum 100