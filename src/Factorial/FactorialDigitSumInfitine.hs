module FactorialDigitSumInfitine(doMain) where
import Data.Char

factorials :: [Integer]
factorials = factorials' 1 0
    where factorials' n i = n : factorials' (n * (i + 1)) (i + 1)

factorial :: Int -> Integer
factorial n = factorials!!n

digitSum :: Integer -> Int
digitSum n = iterateThrough (show n)
    where
        stringToNumbers = map (\x -> ord x - 48)
        iterateThrough str = sum (stringToNumbers str)

factorialDigitSum :: Int -> Int
factorialDigitSum n = digitSum (factorial n)

doMain :: Int
doMain = factorialDigitSum 100