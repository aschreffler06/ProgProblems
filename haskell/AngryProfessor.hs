-- https://www.hackerrank.com/challenges/angry-professor/problem

import Text.Printf (printf)
import Control.Monad (replicateM)

readInts :: IO [Int]
readInts = fmap (fmap read . words) getLine

main = do
  n <- readLn :: IO Int
  cases <- replicateM (n * 2) readInts
  mapM_ putStrLn (solve n cases)

solve :: Int -> [[Int]] -> [String]
solve 0 _ = []
solve n cases = do
  let x = cases !! 0
  let th = x !! 1
  let times = cases !! 1
  let newCases = drop 2 cases
  if length (filter (<= 0) times) < th then "YES":(solve (n-1) newCases) else "NO":(solve (n-1) newCases)
