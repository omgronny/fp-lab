import Data.Char
import Data.List
import Data.Foldable

factorial :: Integer -> Integer
factorial n = tailFactorial 1 n
    where
        tailFactorial acc n
            | n == 0    = acc
            | otherwise = tailFactorial (acc * n) (n - 1)

digitSum :: Integer -> Int
digitSum n = iterateThrough (show n)
    where
        stringToNumbers = map (\x -> ord x - 48)
        iterateThrough str = foldr (+) 0 (stringToNumbers str)

factorialDigitSum :: Integer -> Int
factorialDigitSum n = digitSum (factorial n)

main = print $ factorialDigitSum 100