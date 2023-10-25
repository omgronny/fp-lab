module FactorialDigitSumFoldAndMap(doMain) where

import Data.Char

factorial :: Integer -> Integer
factorial = tailFactorial 1
    where
        tailFactorial :: Integer -> Int -> Integer
        tailFactorial acc 0 = acc
        tailFactorial acc n' = tailFactorial (acc * n') (n' - 1)

digitSum :: Integer -> Int
digitSum n = iterateThrough (show n)
    where
        stringToNumbers = map (\x -> ord x - 48)
        iterateThrough str = sum (stringToNumbers str)

factorialDigitSum :: Integer -> Int
factorialDigitSum n = digitSum (factorial n)

doMain :: Int
doMain = factorialDigitSum 100