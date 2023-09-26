import Data.List (tails, transpose)

parse :: String -> [[Int]]
parse = map (map read . words) . lines

-----------------------------------------------------------------------------------------

getRows :: [a] -> [[a]]
getRows [] = []
getRows row = take 4 row : getRows (tail row)

-----------------------------------------------------------------------------------------

horizontalRows :: [[Int]] -> [[Int]]
horizontalRows = concatMap getRows

-----------------------------------------------------------------------------------------

verticalRows :: [[Int]] -> [[Int]]
verticalRows grid = horizontalRows (transpose grid)

-----------------------------------------------------------------------------------------

doCreateDiagonalListFrom :: [[Int]] -> [[Int]]
doCreateDiagonalListFrom = verticalRows . shiftRows
    where
        shiftRows :: [[Int]] -> [[Int]]
        shiftRows = zipWith drop [0..]

createDiagonalListFrom :: [[Int]] -> [[Int]]
createDiagonalListFrom [] = []
createDiagonalListFrom grid = doCreateDiagonalListFrom grid ++ createDiagonalListFrom (tail grid)

-----------------------------------------------------------------------------------------

getProducts :: [[Int]] -> [Int]
getProducts [] = []
getProducts grid = map product grid

-----------------------------------------------------------------------------------------

main :: IO ()
main = do
    str <- readFile "grid"
    let grid = parse str
    print $ maximum . getProducts $
        horizontalRows grid ++
        verticalRows grid ++
        createDiagonalListFrom grid ++
        createDiagonalListFrom (map reverse grid)
