module Factorial.FactorialDigitSumTailRec(doMain) where

import Data.Char

factorial :: Integer -> Integer
factorial = tailFactorial 1
    where
        tailFactorial acc n'
            | n' == 0    = acc
            | otherwise = tailFactorial (acc * n') (n' - 1)

digitSum :: Integer -> Int
digitSum n = iterateThrough 0 (show n)
    where
        iterateThrough :: Int -> String -> Int
        iterateThrough acc [] = acc
        iterateThrough acc (s:str) = iterateThrough (acc + ord s - 48) str

factorialDigitSum :: Integer -> Int
factorialDigitSum n = digitSum (factorial n)

doMain :: Int
doMain = factorialDigitSum 100