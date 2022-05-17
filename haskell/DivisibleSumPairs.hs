-- https://www.hackerrank.com/challenges/divisible-sum-pairs/problem

import Text.Printf (printf)
import Control.Monad (replicateM)

readInts :: IO [Int]
readInts = fmap (fmap read . words) getLine

main = do
  x <- readInts
  let k = x !! 1
  xs <- readInts
  print(solve xs k)

solve :: [Int] -> Int -> Int
solve xs k = length [(i,j) | i <- [0..length xs - 1], j <- [0.. length xs - 1], i < j, ((xs !! i) + (xs !! j)) `mod` k == 0]
