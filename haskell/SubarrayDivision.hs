-- https://www.hackerrank.com/challenges/the-birthday-bar/problem

import Text.Printf (printf)
import Control.Monad (replicateM)

readInts :: IO [Int]
readInts = fmap (fmap read . words) getLine

main = do
  _ <- getLine
  candy <- readInts
  x <- readInts
  let s = x !! 0
  let n = x !! 1
  print(solve candy n s)

solve :: [Int] -> Int -> Int -> Int
solve [] _ _ = 0
solve (x:xs) n s | length (x:xs) < n = 0
                 | otherwise = if sum (take n (x:xs)) == s then 1 + (solve xs n s) else solve xs n s
