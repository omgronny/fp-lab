module FactorialDigitSumInfitine(doMain) where

import Data.Char

factorial :: Int -> Integer
factorial n = product (take n [1..])

digitSum :: Integer -> Int
digitSum n = iterateThrough (show n)
    where
        stringToNumbers = map (\x -> ord x - 48)
        iterateThrough str = sum (stringToNumbers str)

factorialDigitSum :: Int -> Int
factorialDigitSum n = digitSum (factorial n)

doMain :: IO ()
doMain = print $ factorialDigitSum 100