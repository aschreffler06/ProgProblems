-- https://www.hackerrank.com/challenges/picking-numbers/problem

import Text.Printf (printf)
import Control.Monad (replicateM)
import Data.List

readInts :: IO [Int]
readInts = fmap (fmap read . words) getLine

main = do
  _ <- getLine
  arr <- readInts
  let sorted = sort arr
  print(solve sorted 0)

solve :: [Int] -> Int -> Int
solve [] m = m
solve sorted m = do
  let first = sorted !! 0
  let p1 = first + 1
  let fs = (filter (== first) sorted)
  let ss = (filter (== p1) sorted)
  let lf = length fs
  let ls = length ss
  let nm = lf + ls
  if lf /= 0 && ls == 0 then solve (drop (length fs) sorted) (maxT (lf, m)) else 
    if nm > m then solve (drop (length fs) sorted) nm else solve (drop (length fs) sorted) m

maxT :: (Int, Int) -> Int
maxT (i, j) = if i > j then i else j
