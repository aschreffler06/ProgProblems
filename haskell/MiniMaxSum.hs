-- https://www.hackerrank.com/challenges/mini-max-sum/problem

import Text.Printf (printf)
import Data.List

main = do
  ns <- readInts
  printf "%d %d" (solveMin ns) (solveMax ns)

solveMin ns = sum (tail(reverse(sort ns)))
solveMax ns = sum (drop 1 (sort ns))


readInts :: IO [Int]
readInts = fmap (fmap read . words) getLine
