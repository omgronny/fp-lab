{-# OPTIONS_GHC -Wno-name-shadowing #-}
module LargestProductRec(doMain) where

import Data.List (transpose)
import Grid

-----------------------------------------------------------------------------------------

productRow :: [Int] -> [Int]
productRow [] = []
productRow row = product (take 4 row) : productRow (tail row)

horizontalProducts :: [[Int]] -> [Int]
horizontalProducts grid [] = []
horizontalProducts grid = productRow (head grid) ++ horizontalProducts (tail grid)

-----------------------------------------------------------------------------------------

verticalProducts :: [[Int]] -> [Int]
verticalProducts grid = horizontalProducts (transpose grid)

-----------------------------------------------------------------------------------------

createDiagonalListFrom :: [[Int]] -> Int -> [Int]
createDiagonalListFrom grid startsFrom = doCreateDiagonalListFrom grid startsFrom 0
  where
    doCreateDiagonalListFrom :: [[Int]] -> Int -> Int -> [Int]
    doCreateDiagonalListFrom [] _ _ = []
    doCreateDiagonalListFrom grid startsFrom currentIndex
      | currentIndex >= 4 = []
      | null (drop (currentIndex + startsFrom) (head grid)) = []
      | otherwise = head grid!!(startsFrom + currentIndex) : doCreateDiagonalListFrom (tail grid) startsFrom (currentIndex + 1)

productDiagonalRow :: [[Int]] -> Int -> [Int]
productDiagonalRow grid startsFrom
  | null (drop startsFrom (head grid)) = []
  | otherwise = product (createDiagonalListFrom grid startsFrom) : productDiagonalRow grid (startsFrom + 1)

diagonalProducts :: [[Int]] -> [Int]
diagonalProducts [] = []
diagonalProducts grid = productDiagonalRow grid 0 ++ diagonalProducts (tail grid)

-----------------------------------------------------------------------------------------

doMain :: Int
doMain = let grid = Grid.getGrid in
        maximum $ horizontalProducts grid ++ verticalProducts grid ++ diagonalProducts grid ++ diagonalProducts (map reverse grid)