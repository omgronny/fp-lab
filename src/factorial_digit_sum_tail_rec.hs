import Data.Char
factorial :: Integer -> Integer
factorial n = tailFactorial 1 n
    where
        tailFactorial acc n
            | n == 0    = acc
            | otherwise = tailFactorial (acc * n) (n - 1)  

digitSum :: Integer -> Int
digitSum n = iterateThrough 0 (show n)
    where
        iterateThrough :: Int -> String -> Int
        iterateThrough acc str
            | null str  = acc
            | otherwise = iterateThrough (acc + ord (head str) - 48) (tail str)

factorialDigitSum :: Integer -> Int
factorialDigitSum n = digitSum (factorial n)

main = print $ factorialDigitSum 100