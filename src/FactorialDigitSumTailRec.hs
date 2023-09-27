module FactorialDigitSumTailRec(doMain) where

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
        iterateThrough acc str
            | null str  = acc
            | otherwise = iterateThrough (acc + ord (head str) - 48) (tail str)

factorialDigitSum :: Integer -> Int
factorialDigitSum n = digitSum (factorial n)

doMain :: IO ()
doMain = print $ factorialDigitSum 100