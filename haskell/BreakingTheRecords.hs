-- https://www.hackerrank.com/challenges/breaking-best-and-worst-records/problem

import Text.Printf (printf)
import Control.Monad (replicateM)

readInts :: IO [Int]
readInts = fmap (fmap read . words) getLine

main = do
  _ <- getLine
  scores <- readInts
  let max = findMax scores (head scores)
  let min = findMin scores (head scores)
  printf "%d %d" max min

findMin :: [Int] -> Int -> Int
findMin [] _ = 0
findMin (x:xs) min = if x < min then 1 + findMin xs x else findMin xs min

findMax :: [Int] -> Int -> Int
findMax [] _ = 0
findMax (x:xs) max = if x > max then 1 + findMax xs x else findMax xs max
