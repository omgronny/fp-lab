module LargestProductFoldAndMap(doMain) where

import Data.List (transpose)
import Grid

-----------------------------------------------------------------------------------------

getRows :: [a] -> [[a]]
getRows row
    | null row  = []
    | otherwise = take 4 row : getRows (tail row)

-----------------------------------------------------------------------------------------

horizontalRows :: [[a]] -> [[a]]
horizontalRows = concatMap getRows

-----------------------------------------------------------------------------------------

verticalRows :: [[a]] -> [[a]]
verticalRows grid = horizontalRows (transpose grid)

-----------------------------------------------------------------------------------------

doCreateDiagonalListFrom :: Num a => [[a]] -> [[a]]
doCreateDiagonalListFrom = verticalRows . shiftRows
    where
        shiftRows :: Num a => [[a]] -> [[a]]
        shiftRows = zipWith drop [0..]

createDiagonalListFrom :: Num a => [[a]] -> [[a]]
createDiagonalListFrom grid
    | null grid = []
    | otherwise = doCreateDiagonalListFrom grid ++ createDiagonalListFrom (tail grid)

-----------------------------------------------------------------------------------------

getProducts :: Num a => [[a]] -> [a]
getProducts grid
    | null grid = []
    | otherwise = map product grid

-----------------------------------------------------------------------------------------

doMain :: Int
doMain = let grid = Grid.getGrid in
    maximum . getProducts $
        horizontalRows grid ++
        verticalRows grid ++
        createDiagonalListFrom grid ++
        createDiagonalListFrom (map reverse grid)
