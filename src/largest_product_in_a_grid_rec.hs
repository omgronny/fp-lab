import Data.List (transpose)
import Data.Array (Ix(index))

parse :: String -> [[Int]]
parse = map (map read . words) . lines

-----------------------------------------------------------------------------------------

productRow :: [Int] -> [Int]
productRow row
  | null row = []
  | otherwise = product (take 4 row) : productRow (tail row)

horizontalProducts :: [[Int]] -> [Int]
horizontalProducts grid
  | null grid = []
  | otherwise = productRow (head grid) ++ horizontalProducts (tail grid)

-----------------------------------------------------------------------------------------

verticalProducts :: [[Int]] -> [Int]
verticalProducts grid = horizontalProducts (transpose grid)

-----------------------------------------------------------------------------------------

createDiagonalListFrom :: [[Int]] -> Int -> [Int]
createDiagonalListFrom grid startsFrom = doCreateDiagonalListFrom grid startsFrom 0
  where
    doCreateDiagonalListFrom :: [[Int]] -> Int -> Int -> [Int]
    doCreateDiagonalListFrom grid startsFrom currentIndex
      | null grid = []
      | currentIndex >= 4 = []
      | null (drop (currentIndex + startsFrom) (head grid)) = []
      | otherwise = head grid!!(startsFrom + currentIndex) : doCreateDiagonalListFrom (tail grid) startsFrom (currentIndex + 1)

productDiagonalRow :: [[Int]] -> Int -> [Int]
productDiagonalRow grid startsFrom
  | null (drop startsFrom (head grid)) = []
  | otherwise = product (createDiagonalListFrom grid startsFrom) : productDiagonalRow grid (startsFrom + 1)

diagonalProducts :: [[Int]] -> [Int]
diagonalProducts grid
  | null grid = []
  | otherwise = productDiagonalRow grid 0 ++ diagonalProducts (tail grid)

-----------------------------------------------------------------------------------------

main :: IO ()
main = do
        str <- readFile "grid"
        let grid = parse str
        print $ maximum $ horizontalProducts grid ++ verticalProducts grid ++ diagonalProducts grid ++ diagonalProducts (map reverse grid)